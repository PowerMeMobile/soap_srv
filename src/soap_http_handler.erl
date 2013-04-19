-module(soap_http_handler).

%% Any soap method ignore blink and private fields

%% TODO
%% Implement following methods:
%% - SendBinarySms 			(11,12)
%% - HTTP_SendBinarySms 	(11,12,get,post)
%% Improve HHTP_Authenticate(credits, no originators)
%% add support for defDate MM/DD/YYYY HH:MM
%% add support for messageType
%% add support for rejectedNumbers
%% {{badmatch,{error,socket_closed_remotely}} on large sendSmsReq (2000 recipients)
%% soap:fault on 500 error
%% check for undefined mandatory parameters

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
-include("soap_srv.hrl").

-define(PATH, "/bmsgw/soap/messenger.asmx").

-define(ContentTypeHName, <<"Content-Type">>).

-define(gv(K, PList), proplists:get_value(K, PList)).
-define(gv(K, PList, Default), proplists:get_value(K, PList, Default)).

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
%% Internal
%% ===================================================================

dispatch_rules() ->
	DispatchRaw =
		[{'_', [
			{?PATH ++ "/[...]",	?MODULE, []},
			{'_', soap_ehttp_handler, []}]
		}],
	cowboy_router:compile(DispatchRaw).

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

get_boolean(<<"true">>) -> true;
get_boolean(<<"false">>) -> false.

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
%% HTTP
%% ===================================================================

process(http, "HTTP_SendSms", Req) ->
	{QsVals, Req2} = get_qs_vals(Req),

	CustomerID = ?gv(<<"customerid">>, QsVals),
	UserName = ?gv(<<"username">>, QsVals),
	Pswd = ?gv(<<"userpassword">>, QsVals),
	Originator = ?gv(<<"originator">>, QsVals),
	MsgText = ?gv(<<"smstext">>, QsVals),
	Recipients = ?gv(<<"recipientphone">>, QsVals),
	Flash = get_boolean(?gv(<<"flash">>, QsVals, false)),

	SendSmsReq = #send_sms_req{
		customer_id = CustomerID,
		user_name = UserName,
		password = Pswd,
		originator = soap_utils:addr_to_dto(Originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = MsgText,
		type = latin,
		def_date = undefined,
		flash = Flash
	},
	lager:debug("Got HTTP request -> ~p", [SendSmsReq]),
	{ok, RequestID} = soap_mt_srv:process(SendSmsReq),
	lager:info("Message sucessfully sent [id: ~p]", [RequestID]),

	RawResp =
		<<
		"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
		"<SendResult xmlns=\"http://pmmsoapmessenger.com/\">"
			"<RejectedNumbers/>"
			%% <RejectedNumbers>
			%%   <string>string</string>
			%%   <string>string</string>
			%% </RejectedNumbers>
			"<TransactionID>%transaction_id%</TransactionID>"
			"<NetPoints>POSTPAID</NetPoints>"
		"</SendResult>"
		>>,
	Headers = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
	Resp = binary:replace(RawResp, <<"%transaction_id%">>, RequestID),
	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(http, "SendServiceSms", Req) ->

	{QsVals, Req2} = get_qs_vals(Req),

	CustomerID = ?gv(<<"customerid">>, QsVals),
	UserName = ?gv(<<"username">>, QsVals),
	Pswd = ?gv(<<"userpassword">>, QsVals),
	Originator = ?gv(<<"originator">>, QsVals),
	Recipients = ?gv(<<"recipientphone">>, QsVals),
	ServiceName = ?gv(<<"servicename">>, QsVals),
	ServiceUrl = ?gv(<<"serviceurl">>, QsVals),
	Flash = get_boolean(?gv(<<"flash">>, QsVals, false)),

	SendServiceSmsReq = #send_service_sms_req{
		customer_id = CustomerID,
		user_name = UserName,
		password = Pswd,
		originator = soap_utils:addr_to_dto(Originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		service_name = ServiceName,
		service_url = ServiceUrl,
		type = latin,
		def_date = undefined,
		flash = Flash
	},

	lager:debug("Got HTTP request -> ~p", [SendServiceSmsReq]),
	{ok, RequestID} = soap_mt_srv:process(SendServiceSmsReq),
	lager:info("Message sucessfully sent [id: ~p]", [RequestID]),

	RawResp =
		<<
		"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
		"<SendResult xmlns=\"http://pmmsoapmessenger.com/\">"
			"<RejectedNumbers/>"
			%% <RejectedNumbers>
		    %% 	<string>string</string>
		    %% 	<string>string</string>
			%% </RejectedNumbers>
			"<TransactionID>%transaction_id%</TransactionID>"
			"<NetPoints>POSTPAID</NetPoints>"
		"</SendResult>"
		>>,
	Headers = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
	Resp = binary:replace(RawResp, <<"%transaction_id%">>, RequestID, [global]),
	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};


process(http, "HTTP_KeepAlive", Req) ->

	{QsVals, Req2} = get_qs_vals(Req),

	CustomerID = ?gv(<<"customerid">>, QsVals),
	UserName = ?gv(<<"username">>, QsVals),
	Pswd = ?gv(<<"userpassword">>, QsVals),

	{ok, _Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Pswd),

	Resp =
		<<
		"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
	 	"<CommonResult xmlns=\"http://pmmsoapmessenger.com/\">"
			"<Result>OK</Result>"
		"</CommonResult>"
		>>,
	Headers = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
	{ok, Req3} = cowboy_req:reply(200, Headers, Resp, Req2),
	{ok, Req3, undefined};

process(http, "HTTP_Authenticate", Req) ->

	{QsVals, Req2} = get_qs_vals(Req),

	CustomerID = ?gv(<<"customerid">>, QsVals),
	UserName = ?gv(<<"username">>, QsVals),
	Pswd = ?gv(<<"userpassword">>, QsVals),

	{ok, Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Pswd),

	Originators =
		list_to_binary(
		[<<"<string>", (Addr#addr.addr)/binary, "</string>">> ||
			Addr <- Customer#k1api_auth_response_dto.allowed_sources]),
	Resp =
		<<
		"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
		"<AuthResult xmlns=\"http://pmmsoapmessenger.com/\">"
			"<NetPoints>undefined</NetPoints>"
			"<Originators>"
				"%originators%"
			"</Originators>"
			"<CustomerID>%customer_id%</CustomerID>"
			"<CreditSMS>undefined</CreditSMS>"
			"<CreditMMS>undefined</CreditMMS>"
		"</AuthResult>"
		>>,
	Resp2 = binary:replace(Resp, <<"%customer_id%">>, CustomerID, [global]),
	Resp3 = binary:replace(Resp2, <<"%originators%">>, Originators, [global]),
	Headers = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],

	{ok, Req3} = cowboy_req:reply(200, Headers, Resp3, Req2),
	{ok, Req3, undefined};

%% ===================================================================
%% SOAP
%% ===================================================================

process({Transport, SendSms}, "SendSms", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, User}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}user", 1, SendSms),
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}CustomerID", 1, User),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Name", 1, User),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Password", 1, User),
	{value, {_, _, [Originator]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}originator", 1, SendSms),
	{value, {_, _, [MsgText]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}smsText", 1, SendSms),
	{value, {_, _, [Recipients]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}recipientPhone", 1, SendSms),
	{value, {_, _, [Flash]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}flash", 1, SendSms),

	SendSmsReq = #send_sms_req{
		customer_id = CustomerID,
		user_name = UserName,
		password = Pswd,
		originator = soap_utils:addr_to_dto(Originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = MsgText,
		type = latin,
		def_date = undefined,
		flash = get_boolean(Flash)
	},
	lager:debug("~p: got -> ~p", [Transport, SendSmsReq]),
	{ok, RequestID} = soap_mt_srv:process(SendSmsReq),
	lager:info("Message sucessfully sent [id: ~p]", [RequestID]),
	{Headers, RawResp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
		    		"<SendSmsResponse xmlns=\"http://pmmsoapmessenger.com/\">"
		      			"<SendSmsResult>"
							"<RejectedNumbers/>"
		        			%% <RejectedNumbers>
		          			%% 	<string>string</string>
		          			%% 	<string>string</string>
		        			%% </RejectedNumbers>
		        			"<TransactionID>%transaction_id%</TransactionID>"
		        			"<NetPoints>POSTPAID</NetPoints>"
		      			"</SendSmsResult>"
		    		"</SendSmsResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<SendSmsResponse xmlns=\"http://pmmsoapmessenger.com/\">"
						"<SendSmsResult>"
							"<RejectedNumbers/>"
							%% <RejectedNumbers>
							%% 	<string>string</string>
							%% 	<string>string</string>
							%% </RejectedNumbers>
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</SendSmsResult>"
					"</SendSmsResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,
	Resp = binary:replace(RawResp, <<"%transaction_id%">>, RequestID),
	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined};

process({Transport, Method}, "KeepAlive", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, User}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}user", 1, Method),
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}CustomerID", 1, User),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Name", 1, User),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Password", 1, User),

	{ok, _Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Pswd),

	{Headers, Resp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<KeepAliveResponse xmlns=\"http://pmmsoapmessenger.com/\">"
						"<KeepAliveResult>"
							"<Result>OK</Result>"
						"</KeepAliveResult>"
					"</KeepAliveResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<KeepAliveResponse xmlns=\"http://pmmsoapmessenger.com/\">"
						"<KeepAliveResult>"
							"<Result>OK</Result>"
						"</KeepAliveResult>"
					"</KeepAliveResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,

	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined};

process({Transport, Method}, "HTTP_KeepAlive", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}customerID", 1, Method),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userName", 1, Method),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userPassword", 1, Method),

	{ok, _Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Pswd),

	{Headers, Resp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<HTTP_KeepAliveResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<HTTP_KeepAliveResult>"
							"<Result>OK</Result>"
						"</HTTP_KeepAliveResult>"
					"</HTTP_KeepAliveResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<HTTP_KeepAliveResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<HTTP_KeepAliveResult>"
							"<Result>OK</Result>"
						"</HTTP_KeepAliveResult>"
					"</HTTP_KeepAliveResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,

	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined};

process({Transport, Method}, "HTTP_Authenticate", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}customerID", 1, Method),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userName", 1, Method),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userPassword", 1, Method),

	{ok, Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Pswd),

	Originators =
		list_to_binary(
		[<<"<string>", (Addr#addr.addr)/binary, "</string>">> ||
			Addr <- Customer#k1api_auth_response_dto.allowed_sources]),
	{Headers, Resp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<HTTP_AuthenticateResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<HTTP_AuthenticateResult>"
							"<NetPoints>undefined</NetPoints>"
							"<Originators>"
								"%originators%"
							"</Originators>"
							"<CustomerID>%customer_id%</CustomerID>"
							"<CreditSMS>undefined</CreditSMS>"
							"<CreditMMS>undefined</CreditMMS>"
						"</HTTP_AuthenticateResult>"
					"</HTTP_AuthenticateResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<HTTP_AuthenticateResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<HTTP_AuthenticateResult>"
							"<NetPoints>undefined</NetPoints>"
							"<Originators>"
								"%originators%"
							"</Originators>"
							"<CustomerID>%customer_id%</CustomerID>"
							"<CreditSMS>undefined</CreditSMS>"
							"<CreditMMS>undefined</CreditMMS>"
						"</HTTP_AuthenticateResult>"
					"</HTTP_AuthenticateResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,
	Resp2 = binary:replace(Resp, <<"%customer_id%">>, CustomerID, [global]),
	Resp3 = binary:replace(Resp2, <<"%originators%">>, Originators, [global]),

	{ok, Req2} = cowboy_req:reply(200, Headers, Resp3, Req),
	{ok, Req2, undefined};

process({Transport, Method}, "Authenticate", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, User}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}user", 1, Method),
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}CustomerID", 1, User),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Name", 1, User),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Password", 1, User),

	{ok, Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Pswd),

	Originators =
		list_to_binary(
		[<<"<string>", (Addr#addr.addr)/binary, "</string>">> ||
			Addr <- Customer#k1api_auth_response_dto.allowed_sources]),
	{Headers, Resp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<AuthenticateResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<AuthenticateResult>"
							"<NetPoints>undefined</NetPoints>"
							"<Originators>"
								"%originators%"
							"</Originators>"
							"<CustomerID>%customer_id%</CustomerID>"
							"<CreditSMS>undefined</CreditSMS>"
							"<CreditMMS>undefined</CreditMMS>"
						"</AuthenticateResult>"
					"</AuthenticateResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<AuthenticateResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<AuthenticateResult>"
							"<NetPoints>undefined</NetPoints>"
							"<Originators>"
								"%originators%"
							"</Originators>"
							"<CustomerID>%customer_id%</CustomerID>"
							"<CreditSMS>undefined</CreditSMS>"
							"<CreditMMS>undefined</CreditMMS>"
						"</AuthenticateResult>"
					"</AuthenticateResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,
	Resp2 = binary:replace(Resp, <<"%customer_id%">>, CustomerID, [global]),
	Resp3 = binary:replace(Resp2, <<"%originators%">>, Originators, [global]),

	{ok, Req2} = cowboy_req:reply(200, Headers, Resp3, Req),
	{ok, Req2, undefined};

process({Transport, SendSms2}, "SendSms2", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, User}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}user", 1, SendSms2),
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}CustomerID", 1, User),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Name", 1, User),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}Password", 1, User),
	{value, {_, _, [Originator]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}originator", 1, SendSms2),
	{value, {_, _, [MsgText]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}smsText", 1, SendSms2),
	{value, {_, _, [Base64Recipients]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}recipientPhonesFile", 1, SendSms2),
	{value, {_, _, [Flash]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}flash", 1, SendSms2),

	Recipients = base64:decode(Base64Recipients),

	SendSmsReq = #send_sms_req{
		customer_id = CustomerID,
		user_name = UserName,
		password = Pswd,
		originator = soap_utils:addr_to_dto(Originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = MsgText,
		type = latin,
		def_date = undefined,
		flash = get_boolean(Flash)
	},
	lager:debug("~p: got -> ~p", [Transport, SendSmsReq]),
	{ok, RequestID} = soap_mt_srv:process(SendSmsReq),
	lager:info("Message sucessfully sent [id: ~p]", [RequestID]),
	{Headers, RawResp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<SendSms2Response "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<SendSms2Result>"
							"<RejectedNumbers/>"
							%% "<RejectedNumbers>"
							%% 	"<string>string</string>"
							%% 	"<string>string</string>"
							%% "</RejectedNumbers>"
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</SendSms2Result>"
					"</SendSms2Response>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<SendSms2Response "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<SendSms2Result>"
							"<RejectedNumbers/>"
							%% "<RejectedNumbers>"
							%% 	"<string>string</string>"
							%% 	"<string>string</string>"
							%% "</RejectedNumbers>"
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</SendSms2Result>"
					"</SendSms2Response>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,
	Resp = binary:replace(RawResp, <<"%transaction_id%">>, RequestID),
	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined};

process({Transport, HTTP_SendSms}, "HTTP_SendSms", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}customerID", 1, HTTP_SendSms),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userName", 1, HTTP_SendSms),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userPassword", 1, HTTP_SendSms),
	{value, {_, _, [Originator]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}originator", 1, HTTP_SendSms),
	{value, {_, _, [MsgText]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}smsText", 1, HTTP_SendSms),
	{value, {_, _, [Recipients]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}recipientPhone", 1, HTTP_SendSms),
	{value, {_, _, [Flash]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}flash", 1, HTTP_SendSms),

	SendSmsReq = #send_sms_req{
		customer_id = CustomerID,
		user_name = UserName,
		password = Pswd,
		originator = soap_utils:addr_to_dto(Originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = MsgText,
		type = latin,
		def_date = undefined,
		flash = get_boolean(Flash)
	},
	lager:debug("~p: got -> ~p", [Transport, SendSmsReq]),
	{ok, RequestID} = soap_mt_srv:process(SendSmsReq),
	lager:info("Message sucessfully sent [id: ~p]", [RequestID]),
	{Headers, RawResp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<HTTP_SendSmsResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<HTTP_SendSmsResult>"
							"<RejectedNumbers/>"
							%% "<RejectedNumbers>"
							%% 	"<string>string</string>"
							%% 	"<string>string</string>"
							%% "</RejectedNumbers>"
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</HTTP_SendSmsResult>"
					"</HTTP_SendSmsResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<HTTP_SendSmsResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<HTTP_SendSmsResult>"
							"<RejectedNumbers/>"
							%% "<RejectedNumbers>"
							%% 	"<string>string</string>"
							%% 	"<string>string</string>"
							%% "</RejectedNumbers>"
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</HTTP_SendSmsResult>"
					"</HTTP_SendSmsResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,
	Resp = binary:replace(RawResp, <<"%transaction_id%">>, RequestID),
	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined};

process({Transport, SendServiceSms}, "SendServiceSms", Req) when
												Transport =:= soap12 orelse
												Transport =:= soap11 ->
	{value, {_, _, [CustomerID]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}customerID", 1, SendServiceSms),
	{value, {_, _, [UserName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userName", 1, SendServiceSms),
	{value, {_, _, [Pswd]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}userPassword", 1, SendServiceSms),
	{value, {_, _, [Originator]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}originator", 1, SendServiceSms),
	{value, {_, _, [Recipients]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}recipientPhone", 1, SendServiceSms),
	{value, {_, _, [ServiceName]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}serviceName", 1, SendServiceSms),
	{value, {_, _, [ServiceUrl]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}serviceUrl", 1, SendServiceSms),
	{value, {_, _, [Flash]}} =
		lists:keysearch("{http://pmmsoapmessenger.com/}flash", 1, SendServiceSms),

	SendServiceSmsReq = #send_service_sms_req{
		customer_id = CustomerID,
		user_name = UserName,
		password = Pswd,
		originator = soap_utils:addr_to_dto(Originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		service_name = ServiceName,
		service_url = ServiceUrl,
		type = latin,
		def_date = undefined,
		flash = get_boolean(Flash)
	},

	lager:debug("~p: got -> ~p", [Transport, SendServiceSmsReq]),
	{ok, RequestID} = soap_mt_srv:process(SendServiceSmsReq),
	lager:info("Message sucessfully sent [id: ~p]", [RequestID]),
	{Headers, RawResp} =
	case Transport of
		soap11 ->
			H = [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				"<soap:Body>"
					"<SendServiceSmsResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<SendServiceSmsResult>"
							"<RejectedNumbers/>"
							%% "<RejectedNumbers>"
							%% 	"<string>string</string>"
							%% 	"<string>string</string>"
							%% "</RejectedNumbers>"
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</SendServiceSmsResult>"
					"</SendServiceSmsResponse>"
				"</soap:Body>"
			"</soap:Envelope>"
			>>,
			{H, R};
		soap12 ->
			H = [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}],
			R =
			<<
			"<?xml version=\"1.0\" encoding=\"utf-8\"?>"
			"<soap12:Envelope "
				"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
				"xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">"
				"<soap12:Body>"
					"<SendServiceSmsResponse "
						"xmlns=\"http://pmmsoapmessenger.com/\">"
						"<SendServiceSmsResult>"
							"<RejectedNumbers/>"
							%% "<RejectedNumbers>"
							%% 	"<string>string</string>"
							%% 	"<string>string</string>"
							%% "</RejectedNumbers>"
							"<TransactionID>%transaction_id%</TransactionID>"
							"<NetPoints>POSTPAID</NetPoints>"
						"</SendServiceSmsResult>"
					"</SendServiceSmsResponse>"
				"</soap12:Body>"
			"</soap12:Envelope>"
			>>,
			{H, R}
	end,
	Resp = binary:replace(RawResp, <<"%transaction_id%">>, RequestID),
	{ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
	{ok, Req2, undefined}.
