-module(soap_srv_handlers).

-include("soap_srv_protocol.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_services/include/alley_services.hrl").

-export([handle/1]).

-define(E_SUCCESS, <<"OK">>).
-define(E_AUTHENTICATION, <<"404.2 FAILURE (User is unknown)">>).
-define(E_INTERNAL,        <<"500 FAILURE (Internal server error)">>).
-define(E_NOT_IMPLEMENTED, <<"501 FAILURE (Not implemented)">>).
-define(E_INVALID_RECIPIENTS, <<"400.1 FAILURE (Invalid recipients format)">>).
-define(E_TIMEOUT, <<"504 FAILURE (Request timeout)">>).
-define(E_ORIGINATOR_NOT_FOUND, <<"600.1 Originator for customerID is not found">>).
-define(E_NO_RECIPIENTS, <<"600.4 Phone not specified">>).
-define(E_NO_DEST_ADDRS, <<"FAILURE: All recipient numbers in your message "
    "are either Rejected or Blacklisted">>).
-define(E_INVALID_DEF_DATE, <<"Def Date format is incorrect. "
    "Correct format is YYYYMMDDHHMMSS">>).
-define(E_NO_MESSAGE_BODY, <<"Message Content Is Empty">>).
-define(E_INVALID_REQUEST_ID, <<"SMS ID for status request is incorrect or not specified">>).
-define(E_EMPTY_REQUEST_ID, <<"605.7 The action you requested cannot be performed, "
    "because one of your the required request parameters ('TransactionID') was not supplied.">>).
-define(E_SERVICE_NAME_OR_URL_EXPECTED, <<"Service name and url is expected">>).
-define(E_CREDIT_LIMIT_EXCEEDED, <<"Customer's postpaid credit limit is exceeded">>).

%% ===================================================================
%% API
%% ===================================================================

-spec handle(record()) -> {ok, record()}.
handle(Req) ->
    handle(authenticate, Req).

%% ===================================================================
%% Handlers
%% ===================================================================

handle(authenticate, _Req = #'Authenticate'{user = User}) ->
    CustomerID = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerID, UserName, Password) of
        {ok, Customer} ->
            Originators = [Addr#addr.addr ||
                Addr <- Customer#k1api_auth_response_customer_dto.allowed_sources],
            {ok, #'AuthResult'{
                'Result' = ?E_SUCCESS,
                'NetPoints' = <<"POSTPAID">>,
                'Originators' = Originators,
                'CustomerID' = CustomerID,
                'CreditSMS' = <<"POSTPAID">>
            }};
        {error, Error} ->
            {ok, #'AuthResult'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, Req = #'HTTP_Authenticate'{}) ->
    CustomerID = Req#'HTTP_Authenticate'.customerID,
    UserName = Req#'HTTP_Authenticate'.userName,
    Password = Req#'HTTP_Authenticate'.userPassword,
    case authenticate(CustomerID, UserName, Password) of
        {ok, Customer} ->
            Originators = [Addr#addr.addr ||
                Addr <- Customer#k1api_auth_response_customer_dto.allowed_sources],
            {ok, #'AuthResult'{
                'Result' = ?E_SUCCESS,
                'NetPoints' = <<"POSTPAID">>,
                'Originators' = Originators,
                'CustomerID' = CustomerID,
                'CreditSMS' = <<"POSTPAID">>
            }};
        {error, Error} ->
            {ok, #'AuthResult'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, _Req = #'KeepAlive'{user = User}) ->
    CustomerID = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerID, UserName, Password) of
        {ok, _Customer} ->
            {ok, #'CommonResult'{'Result' = ?E_SUCCESS}};
        {error, Error} ->
            {ok, #'CommonResult'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, Req = #'HTTP_KeepAlive'{}) ->
    CustomerID = Req#'HTTP_KeepAlive'.customerID,
    UserName = Req#'HTTP_KeepAlive'.userName,
    Password = Req#'HTTP_KeepAlive'.userPassword,
    case authenticate(CustomerID, UserName, Password) of
        {ok, _Customer} ->
            {ok, #'CommonResult'{'Result' = ?E_SUCCESS}};
        {error, Error} ->
            {ok, #'CommonResult'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, Req = #'SendSms'{user = User}) ->
    CustomerId = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(check_params, Req, Customer);
        {error, Error} ->
            send_result(#send_result{result = Error})
    end;

handle(authenticate, Req = #'HTTP_SendSms'{}) ->
    CustomerId = Req#'HTTP_SendSms'.'customerID',
    UserName = Req#'HTTP_SendSms'.'userName',
    Password = Req#'HTTP_SendSms'.'userPassword',
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(check_params, Req, Customer);
        {error, Error} ->
            send_result(#send_result{result = Error})
    end;

handle(authenticate, Req = #'SendSms2'{user = User}) ->
    CustomerId = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(check_params, Req, Customer);
        {error, Error} ->
            send_result(#send_result{result = Error})
    end;

handle(authenticate, Req = #'SendServiceSms'{}) ->
    CustomerId = Req#'SendServiceSms'.'customerID',
    UserName = Req#'SendServiceSms'.userName,
    Password = Req#'SendServiceSms'.userPassword,
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(check_params, Req, Customer);
        {error, Error} ->
            send_result(#send_result{result = Error})
    end;

handle(authenticate, Req = #'SendBinarySms'{user = User}) ->
    CustomerId = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(check_params, Req, Customer);
        {error, Error} ->
            send_result(#send_result{result = Error})
    end;

handle(authenticate, Req = #'HTTP_SendBinarySms'{}) ->
    CustomerId = Req#'HTTP_SendBinarySms'.'customerID',
    UserName = Req#'HTTP_SendBinarySms'.'userName',
    Password = Req#'HTTP_SendBinarySms'.'userPassword',
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(check_params, Req, Customer);
        {error, Error} ->
            send_result(#send_result{result = Error})
    end;

handle(authenticate, Req = #'GetSmsStatus'{user = User}) ->
    CustomerId = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerId, UserName, Password) of
        {ok, Customer} ->
            handle(get_sms_status, Req, Customer);
        {error, Error} ->
            {ok, #'SmsStatus'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, Req = #'HTTP_GetSmsStatus'{}) ->
    CustomerId = Req#'HTTP_GetSmsStatus'.customerID,
    UserId = Req#'HTTP_GetSmsStatus'.userName,
    Password = Req#'HTTP_GetSmsStatus'.userPassword,
    case authenticate(CustomerId, UserId, Password) of
        {ok, Customer} ->
            handle(get_sms_status, Req, Customer);
        {error, Error} ->
            {ok, #'SmsStatus'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, Req = #'InboxProcessing'{user = User}) ->
    CustomerId = User#user.'CustomerID',
    UserId = User#user.'Name',
    Password = User#user.'Password',
    case authenticate(CustomerId, UserId, Password) of
        {ok, Customer} ->
            handle(inbox_processing, Req, Customer);
        {error, Error} ->
            {ok, #'CommonResult'{'Result' = reformat_error(Error)}}
    end;

handle(authenticate, Req = #'HTTP_InboxProcessing'{}) ->
    CustomerId = Req#'HTTP_InboxProcessing'.customerID,
    UserId = Req#'HTTP_InboxProcessing'.userName,
    Password = Req#'HTTP_InboxProcessing'.userPassword,
    case authenticate(CustomerId, UserId, Password) of
        {ok, Customer} ->
            handle(inbox_processing, Req, Customer);
        {error, Error} ->
            {ok, #'CommonResult'{'Result' = reformat_error(Error)}}
    end;

handle(_, _) ->
    erlang:error(method_not_implemented).

handle(check_params, Req = #'SendSms'{}, Customer) ->
    DefDate =  Req#'SendSms'.defDate,
    case parse_def_date(DefDate) of
        {ok, ParsedDefDate} ->
            Req2 = Req#'SendSms'{defDate = ParsedDefDate},
            handle(send, Req2, Customer);
        {error, invalid} ->
            send_result(#send_result{result = invalid_def_date})
    end;

handle(check_params, Req = #'HTTP_SendSms'{}, Customer) ->
    DefDate =  Req#'HTTP_SendSms'.defDate,
    case parse_def_date(DefDate) of
        {ok, ParsedDefDate} ->
            Req2 = Req#'HTTP_SendSms'{defDate = ParsedDefDate},
            handle(send, Req2, Customer);
        {error, invalid} ->
            send_result(#send_result{result = invalid_def_date})
    end;

handle(check_params, Req = #'SendSms2'{}, Customer) ->
    try base64:decode(Req#'SendSms2'.recipientPhonesFile) of
        Recipients ->
            Req2 = Req#'SendSms2'{recipientPhonesFile = Recipients},
            DefDate = Req2#'SendSms2'.defDate,
            case parse_def_date(DefDate) of
                {ok, ParsedDefDate} ->
                    Req3 = Req2#'SendSms2'{defDate = ParsedDefDate},
                    handle(send, Req3, Customer);
                {error, invalid} ->
                    send_result(#send_result{result = invalid_def_date})
            end
    catch
        _:_ ->
            ?log_error("Invalid recipientPhonesFile: ~p",
                [Req#'SendSms2'.recipientPhonesFile]),
            send_result(#send_result{result = no_recipients})
    end;

handle(check_params, Req = #'SendServiceSms'{}, Customer) ->
    DefDate =  Req#'SendServiceSms'.defDate,
    case parse_def_date(DefDate) of
        {ok, ParsedDefDate} ->
            Req2 = Req#'SendServiceSms'{defDate = ParsedDefDate},
            handle(send, Req2, Customer);
        {error, invalid} ->
            send_result(#send_result{result = invalid_def_date})
    end;

handle(check_params, Req = #'SendBinarySms'{}, Customer) ->
    DefDate =  Req#'SendBinarySms'.defDate,
    case parse_def_date(DefDate) of
        {ok, ParsedDefDate} ->
            Req2 = Req#'SendBinarySms'{defDate = ParsedDefDate},
            handle(send, Req2, Customer);
        {error, invalid} ->
            send_result(#send_result{result = invalid_def_date})
    end;

handle(check_params, Req = #'HTTP_SendBinarySms'{}, Customer) ->
    DefDate =  Req#'HTTP_SendBinarySms'.defDate,
    case parse_def_date(DefDate) of
        {ok, ParsedDefDate} ->
            Req2 = Req#'HTTP_SendBinarySms'{defDate = ParsedDefDate},
            handle(send, Req2, Customer);
        {error, invalid} ->
            send_result(#send_result{result = invalid_def_date})
    end;

handle(send, Req = #'SendSms'{user = User}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = User#user.'Name',
    Req2 = #send_req{
        action = send_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = soap,
        customer = Customer,
        recipients = reformat_addrs(Req#'SendSms'.recipientPhone),
        originator = reformat_addr(Req#'SendSms'.originator),
        text = Req#'SendSms'.smsText,
        type = Req#'SendSms'.messageType,
        def_date =  Req#'SendSms'.defDate,
        flash = maybe_boolean(Req#'SendSms'.flash)
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
             {ok, #'SendResult'{'Result' = reformat_error(Error)}}
        end;

handle(send, Req = #'HTTP_SendSms'{}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = Req#'HTTP_SendSms'.'userName',
    Req2 = #send_req{
        action = send_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = soap,
        customer = Customer,
        recipients = reformat_addrs(Req#'HTTP_SendSms'.recipientPhone),
        originator = reformat_addr(Req#'HTTP_SendSms'.originator),
        text = Req#'HTTP_SendSms'.smsText,
        type = Req#'HTTP_SendSms'.messageType,
        def_date =  Req#'HTTP_SendSms'.defDate,
        flash = maybe_boolean(Req#'HTTP_SendSms'.flash)
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(send, Req = #'SendSms2'{user = User}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = User#user.'Name',
    Req2 = #send_req{
        action = send_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = soap,
        customer = Customer,
        recipients = reformat_addrs(Req#'SendSms2'.recipientPhonesFile),
        originator = reformat_addr(Req#'SendSms2'.originator),
        text = Req#'SendSms2'.smsText,
        type = Req#'SendSms2'.messageType,
        def_date =  Req#'SendSms2'.defDate,
        flash = maybe_boolean(Req#'SendSms2'.flash)
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(send, Req = #'SendServiceSms'{}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = Req#'SendServiceSms'.userName,
    Req2 = #send_req{
        action = send_service_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = soap,
        customer = Customer,
        recipients = reformat_addrs(Req#'SendServiceSms'.recipientPhone),
        originator = reformat_addr(Req#'SendServiceSms'.originator),
        s_name = Req#'SendServiceSms'.serviceName,
        s_url = Req#'SendServiceSms'.serviceUrl,
        type = Req#'SendServiceSms'.messageType,
        def_date =  Req#'SendServiceSms'.defDate,
        flash = maybe_boolean(Req#'SendServiceSms'.flash)
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(send, Req = #'SendBinarySms'{user = User}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = User#user.'Name',
    Req2 = #send_req{
        action = send_binary_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = soap,
        customer = Customer,
        recipients = reformat_addrs(Req#'SendBinarySms'.recipientPhone),
        originator = reformat_addr(Req#'SendBinarySms'.originator),
        binary_body = Req#'SendBinarySms'.binaryBody,
        def_date = Req#'SendBinarySms'.defDate,
        data_coding = Req#'SendBinarySms'.data_coding,
        esm_class = Req#'SendBinarySms'.esm_class,
        protocol_id = Req#'SendBinarySms'.'PID'
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(send, Req = #'HTTP_SendBinarySms'{}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = Req#'HTTP_SendBinarySms'.'userName',
    Req2 = #send_req{
        action = send_binary_sms,
        customer_id = CustomerId,
        user_id = UserId,
        client_type = soap,
        customer = Customer,
        recipients = reformat_addrs(Req#'HTTP_SendBinarySms'.recipientPhone),
        originator = reformat_addr(Req#'HTTP_SendBinarySms'.originator),
        binary_body = Req#'HTTP_SendBinarySms'.binaryBody,
        def_date = Req#'HTTP_SendBinarySms'.defDate,
        data_coding = Req#'HTTP_SendBinarySms'.data_coding,
        esm_class = Req#'HTTP_SendBinarySms'.esm_class,
        protocol_id = Req#'HTTP_SendBinarySms'.'PID'
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(get_sms_status, Req = #'GetSmsStatus'{user = User}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = User#user.'Name',
    TransactionId = Req#'GetSmsStatus'.transactionID,
    Detailed = maybe_boolean(Req#'GetSmsStatus'.detailed),
    get_sms_status(CustomerId, UserId, TransactionId, Detailed);

handle(get_sms_status, Req = #'HTTP_GetSmsStatus'{}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = Req#'HTTP_GetSmsStatus'.userName,
    TransactionId = Req#'HTTP_GetSmsStatus'.transactionID,
    Detailed = maybe_boolean(Req#'HTTP_GetSmsStatus'.detailed),
    get_sms_status(CustomerId, UserId, TransactionId, Detailed);

handle(inbox_processing, Req = #'InboxProcessing'{user = User}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = User#user.'Name',
    Operation = inbox_operation(Req#'InboxProcessing'.operation),
    MessageIds = Req#'InboxProcessing'.messageId,
    inbox_processing(CustomerId, UserId, Operation, MessageIds);

handle(inbox_processing, Req = #'HTTP_InboxProcessing'{}, Customer) ->
    CustomerId = Customer#k1api_auth_response_customer_dto.uuid,
    UserId = Req#'HTTP_InboxProcessing'.userName,
    Operation = inbox_operation(Req#'HTTP_InboxProcessing'.operation),
    MessageIds = Req#'HTTP_InboxProcessing'.messageId,
    inbox_processing(CustomerId, UserId, Operation, MessageIds);

handle(_, _, _) ->
    erlang:error(method_not_implemented).

%% ===================================================================
%% Internal Handlers
%% ===================================================================

send_result(#send_result{
    result = ok,
    req_id = ReqId,
    rejected = Rejected
}) ->
    {ok, #'SendResult'{
        'Result' = ?E_SUCCESS,
        'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
        'TransactionID' = ReqId,
        'NetPoints' = <<"POSTPAID">>
    }};
send_result(#send_result{result = Result}) ->
    {ok, #'SendResult'{
        'Result' = reformat_error(Result)
    }}.

authenticate(CustomerID, UserName, Password) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            {ok, Customer};
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {error, authentication};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {error, Error}
    end.

get_sms_status(CustomerId, UserId, TransactionId, Detailed) ->
    Addr = alley_services_utils:addr_to_dto(<<>>),
    case alley_services_api:get_delivery_status(
            CustomerId, UserId, TransactionId, Addr) of
        {ok, #k1api_sms_delivery_status_response_dto{statuses = Statuses}} ->
            Statistics = build_statistics(Statuses),
            case Detailed of
                true ->
                    Details = build_details(Statuses),
                    {ok, #'SmsStatus'{
                        'Result' = ?E_SUCCESS,
                        'Statistics' = Statistics,
                        'Details' = Details,
                        'NetPoints' = <<"POSTPAID">>
                    }};
                false ->
                    {ok, #'SmsStatus'{
                        'Result' = ?E_SUCCESS,
                        'Statistics' = Statistics,
                        'NetPoints' = <<"POSTPAID">>
                    }}
            end;
        {error, Error} ->
            ?log_error("GetSmsStatus failed with: ~p", [Error]),
            {ok, #'SmsStatus'{'Result' = reformat_error(Error)}}
    end.

inbox_processing(_CustomerId, _UserId, _Operation, _MessageIds) ->
    %% case alley_services_api:process_inbox(CustomerId, UserId,
    %%         Operation, MessageIds) of
    %%     {ok, #k1api_process_inbox_response_dto{result = Result}} ->
    %%         handle_inbox_response(Result);
    %%     {error, Reason} ->
    %%         {ok, #'CommonResult'{'Result' = Reason}}
    %% end.
    {ok, #'CommonResult'{'Result' = ?E_NOT_IMPLEMENTED}}.

%% ===================================================================
%% Internal
%% ===================================================================

%% handle_inbox_response({messages, _Messages}) ->
%%     {ok, #'CommonResult'{'Result' = <<"messages">>}};
%% handle_inbox_response({deleted, _Deleted}) ->
%%     {ok, #'CommonResult'{'Result' = <<"deleted">>}};
%% handle_inbox_response({error, Error}) ->
%%     {ok, #'CommonResult'{'Result' = Error}}.

inbox_operation(<<"list-all">>)  -> list_all;
inbox_operation(<<"list-new">>)  -> list_new;
inbox_operation(<<"fetch-all">>) -> fetch_all;
inbox_operation(<<"fetch-new">>) -> fetch_new;
inbox_operation(<<"fetch-id">>)  -> fetch_id;
inbox_operation(<<"kill-all">>)  -> kill_all;
inbox_operation(<<"kill-old">>)  -> kill_old;
inbox_operation(<<"kill-id">>)   -> kill_id.

build_details(Statuses) ->
    <<
    "<details xmlns=\"\">",
    (list_to_binary([detailed_status_tag(Status) || Status <- Statuses]))/binary,
    "</details>"
    >>.

detailed_status_tag(Status = #k1api_sms_status_dto{}) ->
    StatusName = Status#k1api_sms_status_dto.status,
    Number = Status#k1api_sms_status_dto.address,
    Timestamp = Status#k1api_sms_status_dto.timestamp,
    ISO8601 = ac_datetime:unixepoch_to_iso8601(Timestamp),

    Content =
    <<
    (content_tag('StatusU', to_utf16_hexdump(StatusName)))/binary,
    (content_tag('number', Number#addr.addr))/binary,
    (content_tag('TimeStamp', ISO8601))/binary
    >>,

    content_tag(reformat_status_name(StatusName), Content).

build_statistics(Statuses) ->
    Agregated = aggregate_statistics(Statuses),
    <<
    "<statistics xmlns=\"\">",
    (list_to_binary([status_tag(reformat_status_name(Status), Counter) || {Status, Counter} <- Agregated]))/binary,
    "</statistics>"
    >>.

status_tag(Status, Counter) ->
    Content =
    <<
    (integer_to_binary(Counter))/binary
    >>,
    content_tag(Status, Content).

content_tag(Name, Content) when is_atom(Name) ->
    content_tag(atom_to_binary(Name, utf8), Content);
content_tag(Name, Content) when is_integer(Content) ->
    content_tag(Name, integer_to_binary(Content));
content_tag(Name, Content) ->
    <<
    "<", Name/binary, ">",
    Content/binary,
    "</", Name/binary, ">"
    >>.

to_utf16_hexdump(Status) ->
    ac_hexdump:binary_to_hexdump(
        unicode:characters_to_binary(
            Status, utf8, utf16), to_lower).

reformat_status_name(Status) when is_binary(Status) ->
     <<"SMSC_", (bstr:upper(Status))/binary>>.

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

reformat_addr(undefined) ->
    reformat_addr(<<"">>);
reformat_addr(Addr) ->
    alley_services_utils:addr_to_dto(Addr).

reformat_addrs(undefined) ->
    [];
reformat_addrs(BlobAddrs) ->
    RawAddrs = binary:split(BlobAddrs, <<",">>, [trim, global]),
    [alley_services_utils:addr_to_dto(Addr) || Addr <- RawAddrs].

reformat_error(authentication) ->
    ?E_AUTHENTICATION;
reformat_error(invalid_def_date) ->
    ?E_INVALID_DEF_DATE;
reformat_error(originator_not_found) ->
    ?E_ORIGINATOR_NOT_FOUND;
reformat_error(no_recipients) ->
    ?E_NO_RECIPIENTS;
reformat_error(no_dest_addrs) ->
    ?E_NO_DEST_ADDRS;
reformat_error(no_message_body) ->
    ?E_NO_MESSAGE_BODY;
reformat_error(invalid_service_name_or_url) ->
    ?E_SERVICE_NAME_OR_URL_EXPECTED;
reformat_error(credit_limit_exceeded) ->
    ?E_CREDIT_LIMIT_EXCEEDED;
reformat_error(empty_request_id) ->
    ?E_EMPTY_REQUEST_ID;
reformat_error(invalid_request_id) ->
    ?E_INVALID_REQUEST_ID;
reformat_error(timeout) ->
    ?E_TIMEOUT;
reformat_error(Result) ->
    atom_to_binary(Result, utf8).

parse_def_date(<<>>) ->
    {ok, undefined};
parse_def_date(undefined) ->
    {ok, undefined};
parse_def_date(DefDateList) when is_list(DefDateList) ->
    try [binary_to_integer(D) || D <- DefDateList] of
        [Month, Day, Year, Hour, Min] ->
            DateTime = {{Year, Month, Day}, {Hour, Min, 0}},
            RefDate = ac_datetime:datetime_to_timestamp(DateTime),
            case RefDate > ac_datetime:utc_timestamp() of
                true ->
                    {ok, RefDate};
                false ->
                    {error, invalid}
            end
    catch
        _:_ ->
            {error, invalid}
    end;
parse_def_date(DefDate) when is_binary(DefDate) ->
    case binary:split(DefDate, [<<"/">>, <<" ">>, <<":">>], [global]) of
        [_M, _D, _Y, _H, _Min] = DefDateList ->
            parse_def_date(DefDateList);
        _ ->
            {error, invalid}
    end.
