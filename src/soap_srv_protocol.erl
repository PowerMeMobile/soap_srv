-module(soap_srv_protocol).

%% Any soap method ignore blink and private fields

%% TODO
%% Add proper response msgs with codes
%% add support for defDate MM/DD/YYYY HH:MM
%% add support for messageType
%% add support for rejectedNumbers
%% <RejectedNumbers>
%% 	<string>string</string>
%% 	<string>string</string>
%% </RejectedNumbers>
%% {{badmatch,{error,socket_closed_remotely}} on large sendSmsReq (2000 recipients)
%% soap:fault on 500 error
%% check for undefined mandatory parameters
%% Imlement independent soap_srv DTO messages

-behaviour(cowboy_http_handler).

%% API
-export([
	init/0,
	update_dispatch_rules/0
]).

%% Cowboy hooks to save req body
-export([
	onrequest_hook/1,
	onresponse_hook/4
]).

%% Cowboy callbacks
-export([
	init/3,
	handle/2,
	terminate/3
]).

-include_lib("alley_dto/include/adto.hrl").
-include("soap_srv_protocol.hrl").
-include("soap_srv.hrl").

-define(Handler, soap_srv_handlers).

-define(PATH, "/bmsgw/soap/messenger.asmx").
-define(NS, "http://pmmsoapmessenger.com/").

-define(ContentTypeHName, <<"Content-Type">>).

-define(gv(K, PList), proplists:get_value(K, PList)).
-define(gv(K, PList, Default), proplists:get_value(K, PList, Default)).


-import(record_info, [record_to_proplist/2]).
-import(record_info, [proplist_to_record/3]).
-include_lib("record_info/include/record_info.hrl").
-export_record_info([
	'HTTP_SendSms',
	'SendSms',
	'SendSms2',
	'SendServiceSms',
	'SendBinarySms',
	'HTTP_SendBinarySms',
	'KeepAlive',
	'HTTP_KeepAlive',
	'Authenticate',
	'HTTP_Authenticate',
	'user',
	'SendResult',
	'CommonResult',
	'AuthResult'
]).

%% ===================================================================
%% API
%% ===================================================================

-spec init() -> ok.
init() ->
	{ok, Port} = application:get_env(http_port),
	ProtocolOpts = [
		{env, [{dispatch, dispatch_rules()}]},
		{max_keepalive, 0}, %% to avoid socket_closed_remotely error
		{onrequest, fun ?MODULE:onrequest_hook/1},
		{onresponse, fun ?MODULE:onresponse_hook/4}
	],
	{ok, _Pid} =
		cowboy:start_http(?MODULE, 100, [{port, Port}], ProtocolOpts),
	lager:info("http_handler: started [0.0.0.0:~p]~n", [Port]),
	ok.

update_dispatch_rules() ->
	cowboy:set_env(?MODULE, dispatch, dispatch_rules()).

%% ===================================================================
%% dispatch rules
%% ===================================================================

dispatch_rules() ->
	DispatchRaw =
		[{'_', [
			{?PATH ++ "/[...]",	?MODULE, []},
			{'_', soap_ehttp_handler, []}]
		}],
	cowboy_router:compile(DispatchRaw).

%% ===================================================================
%% Cowboy hooks
%% ===================================================================

%% Since app uses cowboy onresponse hook for logging purposes
%% and also need http request body in debug mode,
%% to overcome cowboy restriction about request body
%% (read http://ninenines.eu/docs/en/cowboy/HEAD/guide/req)
%% that the request body can only be done once, as it is
%% read directly from the socket, app uses following
%% strategy:
%% 1. Save request body to proc dictionary with onrequest hook
%% 2. Uses get_body() function to get body from dictionary
%% 3. Cleanup req_body with clean_body() function on terminate since
%% the same process can be used to process next request in kepepalive
%% mode (read http://ninenines.eu/docs/en/cowboy/HEAD/guide/internals
%% 'One process for many requests' section)

-spec onrequest_hook(cowboy_req:req()) -> cowboy_req:req().
onrequest_hook(Req) ->
	%% 2k recipients = 28k body for HTTP POST x-www-form-urlencoded
	{ok, Body, Req2} = cowboy_req:body(800000, Req),
	put(req_body, Body),
	Req2.

-spec onresponse_hook(	non_neg_integer(),
						list(), binary(), cowboy_req:req()) ->
		cowboy_req:req().
onresponse_hook(RespCode, RespHeaders, RespBody, Req) ->
	ReqBody = get_body(),
	soap_srv_http_logger:log(RespCode, RespHeaders, RespBody,
								Req, ReqBody),
	{ok, Req2} =
		cowboy_req:reply(RespCode, RespHeaders, RespBody, Req),
	Req2.

%% get body helper
get_body() -> get(req_body).

%% clean body helper
clean_body() -> put(req_body, undefined).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({tcp, http}, Req, []) ->
	case cowboy_req:qs(Req) of
		{<<"WSDL">>, Req2} -> {ok, Req2, wsdl};
		{_, Req2} -> {ok, Req2, undefined}
	end.

handle(Req, wsdl) ->
	{ok, Binary} = file:read_file("./data/WSDL"),
	Scheme = <<"http://">>,
	{Host, Req2} = cowboy_req:host(Req),
	{Path, Req3} = cowboy_req:path(Req2),
	Location = <<Scheme/binary, Host/binary, Path/binary>>,
	Resp = binary:replace(Binary, <<"%%location%%">>, Location, [global]),
	{ok, Req4} = cowboy_req:reply(200, [], Resp, Req3),
	{ok, Req4, undefined};

handle(Req, _State) ->
	{ReqPath, Req} = cowboy_req:path(Req),
	case ReqPath of
		<<?PATH>> ->
			process(soap, undefined, Req);
		<<?PATH, $/, SoapMethod/binary>> ->
			process(http, binary_to_list(SoapMethod), Req)
	end.

terminate(_Reason, _Req, _St) ->
	clean_body(),		%% Need to cleanup body record in proc dict
	ok.					%% since cowboy uses one process per several
						%% requests in keepalive mode

%% ===================================================================
%% Process Requests
%% ===================================================================

process(soap, undefined, Req) ->
	Body = get_body(),
	{ok, SimpleForm, []} = erlsom:simple_form(Body, [{output_encoding, utf8}]),
	case SimpleForm of
		{"{http://schemas.xmlsoap.org/soap/envelope/}Envelope", [], Envelope} ->
			process({soap11, Envelope}, undefined, Req);
		{"{http://www.w3.org/2003/05/soap-envelope}Envelope",[], Envelope} ->
			process({soap12, Envelope}, undefined, Req);
		{error, Error} ->
			lager:error("Xml parse error: ~p", [Error]),
			Resp = <<"Invalid soap message format">>,
			{ok, Req3} = cowboy_req:reply(400, [], Resp, Req),
			{ok, Req3, undefined}
	end;

process({soap11, Envelope}, undefined, Req) ->
	{value, {_, _, [{MethodName, _, MethodBody}]}} =
		lists:keysearch("{http://schemas.xmlsoap.org/soap/envelope/}Body", 1, Envelope),
	NameSpace = "{http://pmmsoapmessenger.com/}",
	process({soap11, MethodBody}, MethodName -- NameSpace, Req);
process({soap12, Envelope}, undefined, Req) ->
	{value, {_, _, [{MethodName, _, MethodBody}]}} =
		lists:keysearch("{http://www.w3.org/2003/05/soap-envelope}Body", 1, Envelope),
	NameSpace = "{http://pmmsoapmessenger.com/}",
	process({soap12, MethodBody}, MethodName -- NameSpace, Req);

%% ===================================================================
%% SOAP Methods
%% ===================================================================

process({Transport, SendSms}, "SendSms", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	Spec = ?MODULE:record_info({keys, 'SendSms'}),
	{MethodPropList, _} = get_method_values({Transport, SendSms}, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'SendSms', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'SendResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'SendResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp = build_response(Transport, "SendSms", Content),

	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined};

process({Transport, Method}, "KeepAlive", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->

	Spec = ?MODULE:record_info({keys, 'KeepAlive'}),
	{MethodPropList, Req2} = get_method_values({Transport, Method}, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'KeepAlive', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'CommonResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'CommonResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp = build_response(Transport, "KeepAlive", Content),

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(Transport, "HTTP_KeepAlive", Req) ->
	Spec = ?MODULE:record_info({keys, 'HTTP_KeepAlive'}),
	{MethodPropList, Req2} = get_method_values(Transport, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'HTTP_KeepAlive', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'CommonResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'CommonResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp =
	case Transport of
		{_,_} -> build_response(Transport, "HTTP_KeepAlive", Content);
		http -> build_response(Transport, "Common", Content)
	end,

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(Transport, "HTTP_Authenticate", Req) ->
	Spec = ?MODULE:record_info({keys, 'HTTP_Authenticate'}),
	{MethodPropList, Req2} = get_method_values(Transport, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'HTTP_Authenticate', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'AuthResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'AuthResult'{'Result' = Explanation}
	end,

	Originators =
	list_to_binary(
		[<<"<string>", Originator/binary, "</string>">> ||
			Originator <- Result#'AuthResult'.'Originators']),
	Result2 = Result#'AuthResult'{'Originators' = Originators},
	Content = build_result_content(Result2),

	Headers = get_headers(Transport),

	Resp =
	case Transport of
		{_,_} -> build_response(Transport, "HTTP_Authenticate", Content);
		http -> build_response(Transport, "Auth", Content)
	end,

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process({Transport, Method}, "Authenticate", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->

	Spec = ?MODULE:record_info({keys, 'Authenticate'}),
	{MethodPropList, Req2} = get_method_values({Transport, Method}, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'Authenticate', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'AuthResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'AuthResult'{'Result' = Explanation}
	end,

	Originators =
	list_to_binary(
		[<<"<string>", Originator/binary, "</string>">> ||
			Originator <- Result#'AuthResult'.'Originators']),
	Result2 = Result#'AuthResult'{'Originators' = Originators},
	Content = build_result_content(Result2),

	Headers = get_headers(Transport),

	Resp = build_response(Transport, "Authenticate", Content),

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process({Transport, SendSms2}, "SendSms2", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->

	Spec = ?MODULE:record_info({keys, 'SendSms2'}),
	{MethodPropList, Req2} = get_method_values({Transport, SendSms2}, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'SendSms2', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'SendResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'SendResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp = build_response(Transport, "SendSms2", Content),
	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(Transport, "HTTP_SendSms", Req) ->
	Spec = ?MODULE:record_info({keys, 'HTTP_SendSms'}),
	{MethodPropList, Req2} = get_method_values(Transport, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'HTTP_SendSms', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'SendResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'SendResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp =
	case Transport of
		{_,_} -> build_response(Transport, "HTTP_SendSms", Content);
		http -> build_response(Transport, "Send", Content)
	end,

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(Transport, "SendServiceSms", Req) ->

	Spec = ?MODULE:record_info({keys, 'SendServiceSms'}),
	{MethodPropList, Req2} = get_method_values(Transport, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'SendServiceSms', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'SendResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'SendResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp =
	case Transport of
		{_,_} -> build_response(Transport, "SendServiceSms", Content);
		http -> build_response(Transport, "Send", Content)
	end,

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process({Transport, SOAPBody}, "SendBinarySms", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->

	Spec = ?MODULE:record_info({keys, 'SendBinarySms'}),
	{MethodPropList, Req2} = get_method_values({Transport, SOAPBody}, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'SendBinarySms', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'SendResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'SendResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp = build_response(Transport, "SendBinarySms", Content),

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(Transport, "HTTP_SendBinarySms", Req) ->

	Spec = ?MODULE:record_info({keys, 'HTTP_SendBinarySms'}),
	{MethodPropList, Req2} = get_method_values(Transport, Spec, Req),
	SOAPReq = proplist_to_record(MethodPropList, 'HTTP_SendBinarySms', ?MODULE),

	Result =
	try ?Handler:handle(SOAPReq) of
		{ok, SOAPResult = #'SendResult'{}} ->
			SOAPResult
	catch
		Class:Error ->
			lager:error("~p:~p", [Class, Error]),
			Explanation = list_to_binary(io_lib:format("~p", [Error])),
			#'SendResult'{'Result' = Explanation}
	end,

	Content = build_result_content(Result),

	Headers = get_headers(Transport),

	Resp =
	case Transport of
		{_,_} -> build_response(Transport, "HTTP_SendBinarySms", Content);
		http -> build_response(Transport, "Send", Content)
	end,

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined}.

%% ===================================================================
%% Internals
%% ===================================================================

build_response(Transport, Method, Content) when is_list(Method) andalso is_binary(Content) ->
	Result = construct_result(Transport, Method, Content),
	Response = construct_response(Transport, Method, Result),
	construct_soap_body(Transport, Response).

construct_result(http, Method, Content) ->
	construct_xml_tag(Method ++ "Result xmlns=\"http://pmmsoapmessenger.com/\"", Content);
construct_result(_Transport, Method, Content) ->
	construct_xml_tag(Method ++ "Result", Content).

construct_response(http, Name, Content) when is_binary(Content) andalso is_list(Name) ->
	Content;
construct_response(_Transport, Name, Content) when is_binary(Content) andalso is_list(Name) ->
	<< "<", (list_to_binary(Name ++ "Respose"))/binary, " xmlns=\"http://pmmsoapmessenger.com/\">",
		Content/binary,
	"</", (list_to_binary(Name ++ "Response"))/binary, ">" >>.

construct_soap_body({Transport, _}, Content) ->
	construct_soap_body(Transport, Content);
construct_soap_body(soap11, Content) when is_binary(Content) ->
	<<
	"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
	"<soap:Envelope "
		"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
		"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
		"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
		"<soap:Body>",
			Content/binary,
		"</soap:Body>"
	"</soap:Envelope>"
	>>;
construct_soap_body(soap12, Content) when is_binary(Content) ->
	<<
	"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
	"<soap12:Envelope "
		"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
		"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
		"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
		"<soap12:Body>",
			Content/binary,
		"</soap12:Body>"
	"</soap12:Envelope>"
	>>;
construct_soap_body(http, Content) when is_binary(Content) ->
	<<
	"<?xml version=\"1.0\" encoding=\"utf-8\"?>",
	Content/binary
	>>.


construct_xml_tag(Name, undefined) ->
	construct_xml_tag(Name, <<>>);
construct_xml_tag(Name, []) ->
	construct_xml_tag(Name, <<>>);
construct_xml_tag(Name, <<>>) when is_atom(Name) ->
	<<"<", (atom_to_binary(Name, utf8))/binary,"/>">>;
construct_xml_tag(Name, <<>>) when is_list(Name) ->
	<<"<", Name, "/>">>;
construct_xml_tag(Name, Content) when is_atom(Name) ->
	<<
	"<",(atom_to_binary(Name, utf8))/binary,">",
	Content/binary,
	"</",(atom_to_binary(Name, utf8))/binary,">">>;
construct_xml_tag(Name, Content) when is_list(Name) ->
	<<
	"<",(list_to_binary(Name))/binary,">",
	Content/binary,
	"</",(list_to_binary(Name))/binary,">">>.

get_headers({Transport, _}) ->
	get_headers(Transport);
get_headers(soap11) ->
	[{?ContentTypeHName, <<"text/xml; charset=utf-8">>}];
get_headers(soap12) ->
	[{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}];
get_headers(http) ->
	[{?ContentTypeHName, <<"text/xml; charset=utf-8">>}].

get_method_values(http, Spec, Req) ->
	{QsVals, Req2} = get_qs_vals(Req),
	Fun =
	fun(AtomKey) ->
		Key = cowboy_bstr:to_lower(atom_to_binary(AtomKey, utf8)),
		Value = ?gv(Key, QsVals),
		{AtomKey, Value}
	end,
	{lists:map(Fun, Spec), Req2};

get_method_values({_,Method}, Spec, Req) ->
	Fun =
	fun(AtomKey) ->
		Key = atom_to_list(AtomKey),
		Value =
		case lists:keysearch("{" ++ ?NS ++ "}" ++ Key, 1, Method) of
			{value, {_, _, [V]}} -> V;
			{value, {_, _, []}} -> undefined;
			{value, {_, _, List}} ->
				SubSpec = ?MODULE:record_info({keys, AtomKey}),
				{Plist, Req} = get_method_values({soap, List}, SubSpec, Req),
				proplist_to_record(Plist, AtomKey, ?MODULE);
			false -> undefined
		end,
		{AtomKey, Value}
	end,
	{lists:map(Fun, Spec), Req}.

get_qs_vals(Req) ->
	{QsVals, Req3} =
	case cowboy_req:method(Req) of
		{<<"GET">>, Req2} ->
			cowboy_req:qs_vals(Req2);
		{<<"POST">>, Req2} ->
			BodyQS = cowboy_http:x_www_form_urlencoded(get_body()),
			{BodyQS, Req2}
	end,
	QsValsLowerCase =
		[{cowboy_bstr:to_lower(K), V} || {K, V} <- QsVals],
	{QsValsLowerCase, Req3}.

build_result_content(Record) when is_tuple(Record) ->
	Plist = record_to_proplist(Record, ?MODULE),
	build_result_content(Plist, []).

build_result_content([], Acc) ->
	list_to_binary(lists:reverse(Acc));
build_result_content([{Key, Value} | Tail], Acc) ->
	Tag = construct_xml_tag(Key, Value),
	build_result_content(Tail, [Tag | Acc]).
