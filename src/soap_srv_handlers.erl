-module(soap_srv_handlers).

-include("soap_srv_protocol.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_services/include/alley_services.hrl").

-export([handle/1]).

-define(INVALID_RECIPIENTS_FORMAT,
    <<"Invalid recipients format. Base64 encoded comma separated address list is expected">>).

%% ===================================================================
%% API
%% ===================================================================

-spec handle(record()) -> {ok, record()}.
handle(Req = #'SendSms'{}) ->
    User = Req#'SendSms'.user,
    Req2 = #send_req{
        action = send_sms,
        customer_id = User#user.'CustomerID',
        user_name = User#user.'Name',
        client_type = soap,
        password = User#user.'Password',
        recipients = reformat_addrs(Req#'SendSms'.recipientPhone),
        originator = reformat_addr(Req#'SendSms'.originator),
        text = Req#'SendSms'.smsText,
        type = Req#'SendSms'.messageType,
        def_date =  Req#'SendSms'.defDate,
        flash = maybe_boolean(Req#'SendSms'.flash)
    },
    {ok, Result} = alley_services_mt:send(Req2),
    ?log_debug("Got submit result: ~p", [Result]),
    send_result(Result);

handle(Req = #'HTTP_SendSms'{}) ->
    Req2 = #send_req{
        action = send_sms,
        customer_id = Req#'HTTP_SendSms'.'customerID',
        user_name = Req#'HTTP_SendSms'.'userName',
        client_type = soap,
        password = Req#'HTTP_SendSms'.'userPassword',
        recipients = reformat_addrs(Req#'HTTP_SendSms'.recipientPhone),
        originator = reformat_addr(Req#'HTTP_SendSms'.originator),
        text = Req#'HTTP_SendSms'.smsText,
        type = Req#'HTTP_SendSms'.messageType,
        def_date =  Req#'HTTP_SendSms'.defDate,
        flash = maybe_boolean(Req#'HTTP_SendSms'.flash)
    },
    {ok, Result} = alley_services_mt:send(Req2),
    ?log_debug("Got submit result: ~p", [Result]),
    send_result(Result);

handle(Req = #'SendSms2'{}) ->
    try base64:decode(Req#'SendSms2'.recipientPhonesFile) of
        Recipients ->
            User = Req#'SendSms2'.user,
            Req2 = #send_req{
                action = send_sms,
                customer_id = User#user.'CustomerID',
                user_name = User#user.'Name',
                client_type = soap,
                password = User#user.'Password',
                recipients = reformat_addrs(Recipients),
                originator = reformat_addr(Req#'SendSms2'.originator),
                text = Req#'SendSms2'.smsText,
                type = Req#'SendSms2'.messageType,
                def_date =  Req#'SendSms2'.defDate,
                flash = maybe_boolean(Req#'SendSms2'.flash)
            },
            {ok, Result} = alley_services_mt:send(Req2),
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result)
    catch
        _:_ ->
            ?log_error("Invalid recipientPhonesFile: ~p",
                [Req#'SendSms2'.recipientPhonesFile]),
            send_result([{result, ?INVALID_RECIPIENTS_FORMAT}])
    end;

handle(Req = #'SendServiceSms'{}) ->
    Req2 = #send_req{
        action = send_service_sms,
        customer_id = Req#'SendServiceSms'.'customerID',
        user_name = Req#'SendServiceSms'.userName,
        client_type = soap,
        password = Req#'SendServiceSms'.userPassword,
        recipients = reformat_addrs(Req#'SendServiceSms'.recipientPhone),
        originator = reformat_addr(Req#'SendServiceSms'.originator),
        s_name = Req#'SendServiceSms'.serviceName,
        s_url = Req#'SendServiceSms'.serviceUrl,
        type = Req#'SendServiceSms'.messageType,
        def_date =  Req#'SendServiceSms'.defDate,
        flash = maybe_boolean(Req#'SendServiceSms'.flash)
    },
    {ok, Result} = alley_services_mt:send(Req2),
    ?log_debug("Got submit result: ~p", [Result]),
    send_result(Result);

handle(Req = #'SendBinarySms'{}) ->
    User = Req#'SendBinarySms'.user,
    Req2 = #send_req{
        action = send_binary_sms,
        customer_id = User#user.'CustomerID',
        user_name = User#user.'Name',
        client_type = soap,
        password = User#user.'Password',
        recipients = reformat_addrs(Req#'SendBinarySms'.recipientPhone),
        originator = reformat_addr(Req#'SendBinarySms'.originator),
        binary_body = Req#'SendBinarySms'.binaryBody,
        def_date = Req#'SendBinarySms'.defDate,
        data_coding = Req#'SendBinarySms'.data_coding,
        esm_class = Req#'SendBinarySms'.esm_class,
        protocol_id = Req#'SendBinarySms'.'PID'
    },
    {ok, Result} = alley_services_mt:send(Req2),
    ?log_debug("Got submit result: ~p", [Result]),
    send_result(Result);

handle(Req = #'HTTP_SendBinarySms'{}) ->
    Req2 = #send_req{
        action = send_binary_sms,
        customer_id = Req#'HTTP_SendBinarySms'.'customerID',
        user_name = Req#'HTTP_SendBinarySms'.'userName',
        client_type = soap,
        password = Req#'HTTP_SendBinarySms'.'userPassword',
        recipients = reformat_addrs(Req#'HTTP_SendBinarySms'.recipientPhone),
        originator = reformat_addr(Req#'HTTP_SendBinarySms'.originator),
        binary_body = Req#'HTTP_SendBinarySms'.binaryBody,
        def_date = Req#'HTTP_SendBinarySms'.defDate,
        data_coding = Req#'HTTP_SendBinarySms'.data_coding,
        esm_class = Req#'HTTP_SendBinarySms'.esm_class,
        protocol_id = Req#'HTTP_SendBinarySms'.'PID'
    },
    {ok, Result} = alley_services_mt:send(Req2),
    ?log_debug("Got submit result: ~p", [Result]),
    send_result(Result);

handle(Req = #'KeepAlive'{}) ->
    User = Req#'KeepAlive'.user,
    CustomerID = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    handle_keep_alive(CustomerID, UserName, Password);

handle(Req = #'HTTP_KeepAlive'{}) ->
    CustomerID = Req#'HTTP_KeepAlive'.customerID,
    UserName = Req#'HTTP_KeepAlive'.userName,
    Password = Req#'HTTP_KeepAlive'.userPassword,
    handle_keep_alive(CustomerID, UserName, Password);

handle(Req = #'HTTP_Authenticate'{}) ->
    CustomerID = Req#'HTTP_Authenticate'.customerID,
    UserName = Req#'HTTP_Authenticate'.userName,
    Password = Req#'HTTP_Authenticate'.userPassword,
    handle_authenticate(CustomerID, UserName, Password);

handle(Req = #'Authenticate'{}) ->
    User = Req#'Authenticate'.user,
    CustomerID = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    handle_authenticate(CustomerID, UserName, Password);

handle(Req = #'HTTP_GetSmsStatus'{}) ->
    CustomerID = Req#'HTTP_GetSmsStatus'.customerID,
    UserName = Req#'HTTP_GetSmsStatus'.userName,
    Password = Req#'HTTP_GetSmsStatus'.userPassword,
    TransactionID = Req#'HTTP_GetSmsStatus'.transactionID,
    Detailed = maybe_boolean(Req#'HTTP_GetSmsStatus'.detailed),
    handle_get_sms_status(CustomerID, UserName, Password, TransactionID, Detailed);

handle(Req = #'GetSmsStatus'{}) ->
    User = Req#'GetSmsStatus'.user,
    CustomerID = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    TransactionID = Req#'GetSmsStatus'.transactionID,
    Detailed = maybe_boolean(Req#'GetSmsStatus'.detailed),
    handle_get_sms_status(CustomerID, UserName, Password, TransactionID, Detailed);

handle(_) ->
    erlang:error(method_not_implemented).

%% ===================================================================
%% Internal
%% ===================================================================

build_details(Statuses) ->
    Details = list_to_binary([detailed_status_tag(Status) || Status <- Statuses]),
    {ok,
    <<
    "<details xmlns=\"\">",
    Details/binary,
    "</details>"
    >>}.

detailed_status_tag(Status = #k1api_sms_status_dto{}) ->
    StatusName = Status#k1api_sms_status_dto.status,
    Number = Status#k1api_sms_status_dto.address,

    Content =
    <<
    (content_tag('number', Number#addr.addr))/binary
    >>,

    content_tag(StatusName, Content).

build_statistics(Statuses) ->
    Agregated = aggregate_statistics(Statuses),
    Statistics = list_to_binary([status_tag(Status, Counter) || {Status, Counter} <- Agregated]),
    {ok,
    <<
    "<statistics xmlns=\"\">",
    Statistics/binary,
    "</statistics>"
    >>}.

status_tag(Status, Counter) ->
    Content =
    <<
    (list_to_binary(integer_to_list(Counter)))/binary
    >>,
    content_tag(Status, Content).

content_tag(Name, Content) when is_atom(Name) ->
    content_tag(atom_to_binary(Name, utf8), Content);
content_tag(Name, Content) when is_integer(Content) ->
    content_tag(Name, list_to_binary(integer_to_list(Content)));
content_tag(Name, Content) ->
    <<
    "<", Name/binary, ">",
    Content/binary,
    "</", Name/binary, ">"
    >>.

aggregate_statistics(Statuses) ->
    aggregate_statistics(Statuses, dict:new()).

aggregate_statistics([], Dict) ->
    dict:to_list(Dict);
aggregate_statistics([#k1api_sms_status_dto{status = Status} | Rest], Dict) ->
    Dict1 = dict:update_counter(Status, 1, Dict),
    aggregate_statistics(Rest, Dict1).

maybe_boolean(<<"true">>)  -> true;
maybe_boolean(<<"false">>) -> false;
maybe_boolean(undefined)   -> false.

reformat_addr(Addr) ->
    alley_services_utils:addr_to_dto(Addr).

reformat_addrs(BlobAddrs) ->
    RawAddrs = binary:split(BlobAddrs, <<",">>, [trim, global]),
    [alley_services_utils:addr_to_dto(Addr) || Addr <- RawAddrs].

send_result(Result) when is_list(Result) ->
    {ok, #'SendResult'{
            'Result' = ?gv(result, Result, <<"OK">>),
            'RejectedNumbers' = [Addr#addr.addr || Addr <- ?gv(rejected, Result, [])],
            'TransactionID' = ?gv(id, Result),
            'NetPoints' = <<"POSTPAID">>
    }}.

handle_authenticate(CustomerID, UserName, Password) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Originators =
                [Addr#addr.addr ||
                    Addr <- Customer#k1api_auth_response_customer_dto.allowed_sources],
            {ok, #'AuthResult'{
                    'Result' = <<"OK">>,
                    'NetPoints' = <<"POSTPAID">>,
                    'Originators' = Originators,
                    'CustomerID' = CustomerID,
                    'CreditSMS' = <<"POSTPAID">>
            }};
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            {ok, #'AuthResult'{'Result' = Error}};
        {error, timeout} ->
            {ok, #'AuthResult'{'Result' = ?authError}}
    end.

handle_keep_alive(CustomerID, UserName, Password) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, _Customer} ->
            {ok, #'CommonResult'{'Result' = <<"OK">>}};
        {error, timeout} ->
            {ok, #'CommonResult'{'Result' = ?authError}}
    end.

handle_get_sms_status(CustomerID, UserName, Password, TransactionID, Detailed) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
            Addr = alley_services_utils:addr_to_dto(<<>>),
            {ok, Response} =
                alley_services_api:get_delivery_status(CustomerUUID, UserName, TransactionID, Addr),
            Statuses = Response#k1api_sms_delivery_status_response_dto.statuses,
            {ok, Statistics} = build_statistics(Statuses),
            case Detailed of
                true ->
                    {ok, Details} = build_details(Statuses),
                    {ok, #'SmsStatus'{
                        'Result' = <<"OK">>,
                        'Statistics' = Statistics,
                        'Details' = Details,
                        'NetPoints' = <<"POSTPAID">>
                    }};
                false ->
                    {ok, #'SmsStatus'{
                        'Result' = <<"OK">>,
                        'Statistics' = Statistics,
                        'NetPoints' = <<"POSTPAID">>
                    }}
            end;
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            {ok, #'AuthResult'{'Result' = Error}};
        {error, Error} ->
            ?log_error("handler: error on auth: ~p", [Error]),
            {ok, #'AuthResult'{'Result' = ?authError}}
    end.
