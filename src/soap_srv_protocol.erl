-module(soap_srv_protocol).

%% Any soap method ignore blink and private fields

%% TODO
%% Add proper response msgs with codes
%% soap:fault on 500 error
%% check for undefined mandatory parameters
%% Imlement independent soap_srv DTO messages

-behaviour(cowboy_http_handler).

-ignore_xref([{update_dispatch_rules, 0}]).

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

-include("logging.hrl").
-include("soap_srv.hrl").
-include("application.hrl").
-include("soap_srv_protocol.hrl").
-include_lib("alley_dto/include/adto.hrl").

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
    'AuthResult',
    'HTTP_GetSmsStatus',
    'SmsStatus'
]).

-type action() ::
    'HTTP_SendSms' |
    'SendSms' |
    'SendSms2' |
    'SendServiceSms' |
    'SendBinarySms' |
    'HTTP_SendBinarySms' |
    'KeepAlive' |
    'HTTP_KeepAlive' |
    'Authenticate' |
    'HTTP_Authenticate'.

-type transport() ::
    soap11 |
    soap12 |
    http_post |
    http_get.

-record(st, {
    http_method :: post | get,
    ct          :: binary(),
    subpath     :: binary(),
    transport   :: transport(),
    xml         :: any(),
    envelope    :: any(),
    action      :: action(),
    action_body :: any(),
    req         :: any(),
    result      :: tuple()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init() -> ok.
init() ->
    {ok, Addr} = application:get_env(?APP, http_addr),
    {ok, Port} = application:get_env(?APP, http_port),
    {ok, AcceptorsNum} = application:get_env(?APP, http_acceptors_num),

    TransOpts = [{ip, Addr}, {port, Port}],
    ProtocolOpts = [
        {env, [{dispatch, dispatch_rules()}]},
        {onrequest, fun ?MODULE:onrequest_hook/1},
        {onresponse, fun ?MODULE:onresponse_hook/4}
    ],

    {ok, _Pid} =
        cowboy:start_http(?MODULE, AcceptorsNum, TransOpts, ProtocolOpts),
    ?log_info("http server is listening to ~p:~p", [Addr, Port]),
    ok.

-spec update_dispatch_rules() -> ok.
update_dispatch_rules() ->
    cowboy:set_env(?MODULE, dispatch, dispatch_rules()).

%% ===================================================================
%% dispatch rules
%% ===================================================================

dispatch_rules() ->
    DispatchRaw =
        [{'_', [
            {?PATH ++ "/[...]", ?MODULE, []},
            {'_', ?MODULE, error}]
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

-spec onresponse_hook(non_neg_integer(),
    list(), binary(), cowboy_req:req()) -> cowboy_req:req().
onresponse_hook(RespCode, RespHeaders, RespBody, Req) ->
    ReqBody = get_body(),
    soap_srv_http_in_logger:log(
        RespCode, RespHeaders, RespBody, Req, ReqBody),
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

-spec init({tcp, http}, cowboy_req:req(), [] | error) ->
    {ok, cowboy_req:req(), #st{} | error}.
init({tcp, http}, Req, []) ->
    {ok, Req, #st{}};
init({tcp, http}, Req, error) ->
    {ok, Req, error}.

-spec handle(cowboy_req:req(), #st{} | error) ->
    {ok, cowboy_req:req(), #st{} | error}.
handle(Req, error) ->
    Resp = <<"Not found: mistake in the host or path of the service URI">>,
    {ok, Req2} = cowboy_req:reply(404, [], Resp, Req),
    {ok, Req2, error};
handle(Req, State) ->
    step(get_http_method, Req, State).

-spec terminate(term(), cowboy_req:req(), #st{}) -> ok.
terminate(_Reason, _Req, _St) ->
    clean_body(),       %% Need to cleanup body record in proc dict
    ok.                 %% since cowboy uses one process per several
                        %% requests in keepalive mode

%% ===================================================================
%% Internals
%% ===================================================================

step(get_http_method, Req, St) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            step(get_subpath, Req2, St#st{http_method = get});
        {<<"POST">>, Req2} ->
            step(get_content_type, Req2, St#st{http_method = post});
        {_Method, _Req2} -> %% @todo implement soap fault
            erlang:error(method_not_supported)
    end;

step(get_content_type, Req, St) ->
    {ok, CTTuple, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
    case tuple_to_list(CTTuple) of
        [<<"application">>, <<"soap+xml">> | _] ->
            step(get_subpath, Req2, St#st{ct = <<"application/soap+xml">>});
        [<<"text">>, <<"xml">> | _] ->
            step(get_subpath, Req2, St#st{ct = <<"text/xml">>});
        [<<"application">>, <<"x-www-form-urlencoded">> | _] ->
            step(get_subpath, Req2, St#st{ct = <<"application/x-www-form-urlencoded">>});
        _ -> erlang:error(unexpected_content_type) %% @todo implement soap fault
    end;

step(get_subpath, Req, St) ->
    {ReqPath, Req2} = cowboy_req:path(Req),
    case ReqPath of
        <<?PATH>> ->
            step(get_transport_type, Req2, St#st{subpath = <<>>});
        <<?PATH, $/, SubPath/binary>> ->
            step(get_transport_type, Req2, St#st{subpath = SubPath})
    end;

step(get_transport_type, Req, St) ->
    case {St#st.http_method, St#st.ct, St#st.subpath} of
        {post, <<"text/xml">>, <<>>} ->
            %% probably soap11
            step(parse_xml, Req, St#st{transport = soap11});
        {post, <<"application/soap+xml">>, <<>>} ->
            %% probably soap12
            step(parse_xml, Req, St#st{transport = soap12});
        {post, <<"application/x-www-form-urlencoded">>, Subpath} when Subpath =/= <<>> ->
            %% probably http_post
            step(get_action_name, Req, St#st{transport = http_post});
        {get, undefined, Subpath} when Subpath =/= <<>> ->
            %% probably http_get
            step(get_action_name, Req, St#st{transport = http_get});
        _ ->
            step(maybe_wsdl_req, Req, St)
    end;

step(parse_xml, Req, St = #st{}) ->
    Body = get_body(),
    try erlsom:simple_form(Body, [{output_encoding, utf8}]) of
        {ok, SimpleForm, _} ->
            step(get_soap_envelope, Req, St#st{xml = SimpleForm})
    catch
        _Class:_Error -> erlang:error(bad_xml) %% @todo implement soap fault
    end;

step(get_soap_envelope, Req, St = #st{}) ->
    SOAP11 = "{http://schemas.xmlsoap.org/soap/envelope/}Envelope",
    SOAP12 = "{http://www.w3.org/2003/05/soap-envelope}Envelope",
    case St#st.xml of
        {SOAP11, _, Envelope} when St#st.transport =:= soap11 ->
            step(get_soap_body, Req, St#st{envelope = Envelope});
        {SOAP12, _, Envelope} when St#st.transport =:= soap12 ->
            step(get_soap_body, Req, St#st{envelope = Envelope});
        {error, Error} -> %% @todo implement soap fault
            ?log_error("Xml parse error: ~p", [Error]),
            Resp = <<"Invalid soap message format">>,
            {ok, Req3} = cowboy_req:reply(400, [], Resp, Req),
            {ok, Req3, undefined};
        _ -> erlang:error(unexpected_xml_format) %% @todo implement soap fault
    end;

step(get_soap_body, Req, St = #st{transport = soap11}) ->
    Body11 = "{http://schemas.xmlsoap.org/soap/envelope/}Body",
    case lists:keysearch(Body11, 1, St#st.envelope) of
        {value, {_, _, [{ActionNameNS, _, ActionBody}]}} ->
            step(get_action_name, Req, St#st{action_body = ActionBody,
                                             action = ActionNameNS});
        _ ->
            erlang:error(soap_body_not_found)
    end;

step(get_soap_body, Req, St = #st{transport = soap12}) ->
    Body12 = "{http://www.w3.org/2003/05/soap-envelope}Body",
    case lists:keysearch(Body12, 1, St#st.envelope) of
        {value, {_, _, [{ActionNameWithNS, _, ActionBody}]}} ->
            step(get_action_name, Req, St#st{action_body = ActionBody,
                                             action = ActionNameWithNS});
        _ ->
            erlang:error(soap_body_not_found) %% @todo implement soap fault
    end;

step(get_action_name, Req, St = #st{transport = Transport}) when
            Transport =:= http_get orelse
            Transport =:= http_post ->
    case action(binary_to_list(St#st.subpath)) of
        undefined ->
            erlang:error(action_not_implemented); %% @todo implement soap fault
        Action ->
            step(is_transport_allowed, Req, St#st{action = Action})
    end;

step(get_action_name, Req, St = #st{transport = SOAP}) when
            SOAP =:= soap11 orelse
            SOAP =:= soap12 ->
    NameSpace = "{" ++ ?NS ++ "}",
    case action(St#st.action --  NameSpace) of
        undefined ->
            erlang:error(action_not_implemented); %% @todo implement soap fault
        Action ->
            step(is_transport_allowed, Req, St#st{action = Action})
    end;

step(is_transport_allowed, Req, St = #st{}) ->
    Spec = [
        {'SendSms', [
            {transport, [soap11, soap12]}
        ]},
        {'SendSms2', [
            {transport, [soap11, soap12]}
        ]},
        {'HTTP_SendSms', [
            {transport, [soap11, soap12, http_get, http_post]}
        ]},
        {'KeepAlive', [
            {transport, [soap11, soap12]}
        ]},
        {'HTTP_KeepAlive', [
            {transport, [soap11, soap12, http_get, http_post]}
        ]},
        {'HTTP_Authenticate',[
            {transport, [soap11, soap12, http_get, http_post]}
        ]},
        {'Authenticate', [
            {transport, [soap11, soap12]}
        ]},
        {'SendServiceSms', [
            {transport, [soap11, soap12, http_get, http_post]}
        ]},
        {'SendBinarySms', [
            {transport, [soap11, soap12]}
        ]},
        {'HTTP_SendBinarySms', [
            {transport, [soap11, soap12, http_get, http_post]}
        ]},
        {'HTTP_GetSmsStatus', [
            {transport, [soap11, soap12, http_get, http_post]}
        ]}
    ],
    ActionSpec = proplists:get_value(St#st.action, Spec),
    AllowedTransports = proplists:get_value(transport, ActionSpec),
    case lists:member(St#st.transport, AllowedTransports) of
        true -> step(get_action_values, Req, St);
        false -> erlang:error(transport_not_allowed) %% @todo implement soap fault
    end;

step(get_action_values, Req, St = #st{}) ->
    Keys = ?MODULE:record_info({keys, St#st.action}),
    {ValuesPropList, Req2} =
        get_method_values(St#st.transport, St#st.action_body, Keys, Req),
    RecordReq = proplist_to_record(ValuesPropList, St#st.action, ?MODULE),
    step(handle, Req2, St#st{req = RecordReq});

step(handle, Req, St = #st{}) ->
    try ?Handler:handle(St#st.req) of
        {ok, Result} when is_tuple(Result) ->
            step(compose_response, Req, St#st{result = Result})
    catch
        Class:Error -> %% @todo implement soap fault
            ?log_error("~p:~p", [Class, Error]),
            Explanation = list_to_binary(io_lib:format("~p", [Error])),
            #'CommonResult'{'Result' = Explanation}
    end;

step(compose_response, Req, St = #st{}) ->
    ResultContent = build_result_content(St#st.result),
    WrapedResult = wrap_result(ResultContent, St),
    Response = construct_response(WrapedResult, St),
    Resp = construct_soap_body(Response, St),
    Headers = get_headers(St#st.transport),
    {ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
    {ok, Req2, undefined};

step(maybe_wsdl_req, Req, St) ->
    {QS, Req2} = cowboy_req:qs(Req),
    case {cowboy_bstr:to_lower(QS), St#st.subpath} of
        {<<"wsdl">>, <<>>} ->
            step(process_wsdl_req, Req2, St);
        _ ->
            erlang:error(not_wsdl) %% @todo implement soap fault
    end;

step(process_wsdl_req, Req, St) ->
    {ok, Binary} = file:read_file("./data/WSDL"),
    Scheme = <<"http://">>,
    {Host, Req2} = cowboy_req:host(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    Location = <<Scheme/binary, Host/binary, Path/binary>>,
    Resp = binary:replace(Binary, <<"%%location%%">>, Location, [global]),
    {ok, Req4} = cowboy_req:reply(200, [], Resp, Req3),
    {ok, Req4, St};

step(_, _, _) ->
    erlang:error(not_implemented). %% @todo implement soap fault


build_result_content(Record) when is_tuple(Record) ->
    Plist = record_to_proplist(Record, ?MODULE),
    build_result_content(Plist, []).

build_result_content([], Acc) ->
    list_to_binary(lists:reverse(Acc));
build_result_content([{Key, Value} | Tail], Acc) when
                    is_list(Value) andalso
                    Value =/= [] ->
    ComposedElements =
    list_to_binary(
        [<<"<string>", Element/binary, "</string>">> ||
            Element <- Value]),
    build_result_content([{Key, ComposedElements} | Tail], Acc);
build_result_content([{Key, Value} | Tail], Acc) ->
    Tag = construct_xml_tag(Key, Value),
    build_result_content(Tail, [Tag | Acc]).

wrap_result(Content, St) when
            St#st.transport =:= http_post orelse
            St#st.transport =:= http_get ->
    Name = atom_to_binary(element(1, St#st.result), utf8),
    NameNS = <<Name/binary, " xmlns=\"", ?NS, "\"">>,
    <<
    "<", NameNS/binary, ">",
    Content/binary,
    "</", Name/binary, ">"
    >>;
wrap_result(Content, St) when
            St#st.transport =:= soap11 orelse
            St#st.transport =:= soap12 ->
    construct_xml_tag(atom_to_list(St#st.action) ++ "Result", Content).

construct_response(Content, St) when
            St#st.transport =:= http_post orelse
            St#st.transport =:= http_get ->
    Content;
construct_response(Content, St) when
            St#st.transport =:= soap11 orelse
            St#st.transport =:= soap12 ->
    Name = atom_to_list(St#st.action),
    << "<", (list_to_binary(Name ++ "Respose"))/binary, " xmlns=\"", ?NS, "\">",
        Content/binary,
    "</", (list_to_binary(Name ++ "Response"))/binary, ">" >>.

construct_soap_body(Content, St) when St#st.transport =:= soap11 ->
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
construct_soap_body(Content, St) when St#st.transport =:= soap12 ->
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
construct_soap_body(Content, St) when
            St#st.transport =:= http_get orelse
            St#st.transport =:= http_post ->
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

get_headers(soap12) ->
    [{?ContentTypeHName, <<"application/soap+xml; charset=utf-8">>}];
get_headers(Transport) when
            Transport =:= http_get orelse
            Transport =:= http_post orelse
            Transport =:= soap11 ->
    [{?ContentTypeHName, <<"text/xml; charset=utf-8">>}].

get_method_values(Transport, _, Keys, Req) when
                Transport =:= http_post orelse
                Transport =:= http_get ->
    {QsVals, Req2} = get_qs_vals(Req),
    Fun = fun(AtomKey) ->
        Key = cowboy_bstr:to_lower(atom_to_binary(AtomKey, utf8)),
        Value = ?gv(Key, QsVals),
        {AtomKey, Value}
    end,
    {lists:map(Fun, Keys), Req2};

get_method_values(Transport, Body, Keys, Req) when
                Transport =:= soap11 orelse
                Transport =:= soap12 ->
    Fun = fun(AtomKey) ->
        Key = atom_to_list(AtomKey),
        Value = case lists:keysearch("{" ++ ?NS ++ "}" ++ Key, 1, Body) of
            {value, {_, _, [V]}} -> V;
            {value, {_, _, []}} -> undefined;
            {value, {_, _, List}} ->
                SubKeys = ?MODULE:record_info({keys, AtomKey}),
                {Plist, Req} = get_method_values(Transport, List, SubKeys, Req),
                proplist_to_record(Plist, AtomKey, ?MODULE);
            false -> undefined
        end,
        {AtomKey, Value}
    end,
    {lists:map(Fun, Keys), Req}.

get_qs_vals(Req) ->
    {QsVals, Req3} = case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            cowboy_req:qs_vals(Req2);
        {<<"POST">>, Req2} ->
            BodyQs = cow_qs:parse_qs(get_body()),
            {BodyQs, Req2}
    end,
    QsValsLowerCase =
        [{cowboy_bstr:to_lower(K), V} || {K, V} <- QsVals],
    {QsValsLowerCase, Req3}.

action("HTTP_SendSms") -> 'HTTP_SendSms';
action("SendSms") -> 'SendSms';
action("SendSms2") -> 'SendSms2';
action("SendServiceSms") -> 'SendServiceSms';
action("SendBinarySms") -> 'SendBinarySms';
action("HTTP_SendBinarySms") -> 'HTTP_SendBinarySms';
action("KeepAlive") -> 'KeepAlive';
action("HTTP_KeepAlive") -> 'HTTP_KeepAlive';
action("Authenticate") -> 'Authenticate';
action("HTTP_Authenticate") -> 'HTTP_Authenticate';
action("HTTP_GetSmsStatus") -> 'HTTP_GetSmsStatus';
action(_) -> undefined.
