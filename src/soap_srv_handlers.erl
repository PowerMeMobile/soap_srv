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
handle(Req = #'SendSms'{}) ->
    User = Req#'SendSms'.user,
    CustomerId = User#user.'CustomerID',
    UserName = User#user.'Name',
    ClientType = soap,
    Password = User#user.'Password',
    case alley_services_auth:authenticate(CustomerId, UserName, ClientType, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Req2 = #send_req{
                action = send_sms,
                customer_id = CustomerId,
                user_name = UserName,
                client_type = ClientType,
                password = Password,
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
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'SendResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(Req = #'HTTP_SendSms'{}) ->
    CustomerId = Req#'HTTP_SendSms'.'customerID',
    UserName = Req#'HTTP_SendSms'.'userName',
    ClientType = soap,
    Password = Req#'HTTP_SendSms'.'userPassword',
    case alley_services_auth:authenticate(CustomerId, UserName, ClientType, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Req2 = #send_req{
                action = send_sms,
                customer_id = CustomerId,
                user_name = UserName,
                client_type = ClientType,
                password = Password,
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
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'SendResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(Req = #'SendSms2'{}) ->
    try base64:decode(Req#'SendSms2'.recipientPhonesFile) of
        Recipients ->
            User = Req#'SendSms2'.user,
            CustomerId = User#user.'CustomerID',
            UserName = User#user.'Name',
            ClientType = soap,
            Password = User#user.'Password',
            case alley_services_auth:authenticate(CustomerId, UserName, ClientType, Password) of
                {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
                    Req2 = #send_req{
                        action = send_sms,
                        customer_id = CustomerId,
                        user_name = UserName,
                        client_type = ClientType,
                        password = Password,
                        customer = Customer,
                        recipients = reformat_addrs(Recipients),
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
                {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
                    ?log_error("Authenticate response error: ~p", [Error]),
                    {ok, #'SendResult'{'Result' = ?E_AUTHENTICATION}};
                {error, Error} ->
                    ?log_error("Authenticate failed with: ~p", [Error]),
                    {ok, #'SendResult'{'Result' = reformat_error(Error)}}
            end
    catch
        _:_ ->
            ?log_error("Invalid recipientPhonesFile: ~p",
                [Req#'SendSms2'.recipientPhonesFile]),
            send_result(#send_result{result = no_recipients})
    end;

handle(Req = #'SendServiceSms'{}) ->
    CustomerId = Req#'SendServiceSms'.'customerID',
    UserName = Req#'SendServiceSms'.userName,
    ClientType = soap,
    Password = Req#'SendServiceSms'.userPassword,
    case alley_services_auth:authenticate(CustomerId, UserName, ClientType, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Req2 = #send_req{
                action = send_service_sms,
                customer_id = CustomerId,
                user_name = UserName,
                client_type = ClientType,
                password = Password,
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
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'SendResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(Req = #'SendBinarySms'{}) ->
    User = Req#'SendBinarySms'.user,
    CustomerId = User#user.'CustomerID',
    UserName = User#user.'Name',
    ClientType = soap,
    Password = User#user.'Password',
    case alley_services_auth:authenticate(CustomerId, UserName, ClientType, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Req2 = #send_req{
                action = send_binary_sms,
                customer_id = CustomerId,
                user_name = UserName,
                client_type = ClientType,
                password = Password,
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
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'SendResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

handle(Req = #'HTTP_SendBinarySms'{}) ->
    CustomerId = Req#'HTTP_SendBinarySms'.'customerID',
    UserName = Req#'HTTP_SendBinarySms'.'userName',
    ClientType = soap,
    Password = Req#'HTTP_SendBinarySms'.'userPassword',
    case alley_services_auth:authenticate(CustomerId, UserName, ClientType, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Req2 = #send_req{
                action = send_binary_sms,
                customer_id = CustomerId,
                user_name = UserName,
                client_type = ClientType,
                password = Password,
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
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'SendResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'SendResult'{'Result' = reformat_error(Error)}}
    end;

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

handle(Req = #'HTTP_InboxProcessing'{}) ->
    CustomerID = Req#'HTTP_InboxProcessing'.customerID,
    UserName = Req#'HTTP_InboxProcessing'.userName,
    Password = Req#'HTTP_InboxProcessing'.userPassword,
    Operation = Req#'HTTP_InboxProcessing'.operation,
    MessageId = Req#'HTTP_InboxProcessing'.messageId,
    handle_inbox_processing(CustomerID, UserName, Password, Operation, MessageId);

handle(Req = #'InboxProcessing'{}) ->
    User = Req#'InboxProcessing'.user,
    CustomerID = User#user.'CustomerID',
    UserName = User#user.'Name',
    Password = User#user.'Password',
    Operation = Req#'InboxProcessing'.operation,
    MessageId = Req#'InboxProcessing'.messageId,
    handle_inbox_processing(CustomerID, UserName, Password, Operation, MessageId);

handle(_) ->
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

handle_authenticate(CustomerID, UserName, Password) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Originators =
                [Addr#addr.addr ||
                    Addr <- Customer#k1api_auth_response_customer_dto.allowed_sources],
            {ok, #'AuthResult'{
                'Result' = ?E_SUCCESS,
                'NetPoints' = <<"POSTPAID">>,
                'Originators' = Originators,
                'CustomerID' = CustomerID,
                'CreditSMS' = <<"POSTPAID">>
            }};
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'AuthResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'AuthResult'{'Result' = ?E_AUTHENTICATION}}
    end.

handle_keep_alive(CustomerID, UserName, Password) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, _Customer}}} ->
            {ok, #'CommonResult'{'Result' = ?E_SUCCESS}};
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'CommonResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'CommonResult'{'Result' = ?E_AUTHENTICATION}}
    end.

handle_get_sms_status(CustomerID, UserName, Password, TransactionID, Detailed) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
            Addr = alley_services_utils:addr_to_dto(<<>>),
            case alley_services_api:get_delivery_status(
                    CustomerUUID, UserName, TransactionID, Addr) of
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
            end;
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'SmsStatus'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'SmsStatus'{'Result' = reformat_error(Error)}}
    end.

handle_inbox_processing(CustomerID, UserName, Password, _Operation, _MessageIds) ->
    case alley_services_auth:authenticate(CustomerID, UserName, soap, Password) of
        {ok, #k1api_auth_response_dto{result = {customer, _Customer}}} ->
            {ok, #'CommonResult'{'Result' = ?E_NOT_IMPLEMENTED}};
            %% CustomerUUID = Customer#k1api_auth_response_customer_dto.uuid,
            %% Operation2 = inbox_operation(Operation),
            %% case alley_services_api:process_inbox(CustomerUUID, UserName,
            %%         Operation2, MessageIds) of
            %%     {ok, #k1api_process_inbox_response_dto{result = Result}} ->
            %%         handle_inbox_response(Result);
            %%     {error, Reason} ->
            %%         {ok, #'CommonResult'{'Result' = Reason}}
            %% end;
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            ?log_error("Authenticate response error: ~p", [Error]),
            {ok, #'AuthResult'{'Result' = ?E_AUTHENTICATION}};
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {ok, #'AuthResult'{'Result' = ?E_AUTHENTICATION}}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% handle_inbox_response({messages, _Messages}) ->
%%     {ok, #'CommonResult'{'Result' = <<"messages">>}};
%% handle_inbox_response({deleted, _Deleted}) ->
%%     {ok, #'CommonResult'{'Result' = <<"deleted">>}};
%% handle_inbox_response({error, Error}) ->
%%     {ok, #'CommonResult'{'Result' = Error}}.

%% inbox_operation(<<"list-all">>)  -> list_all;
%% inbox_operation(<<"list-new">>)  -> list_new;
%% inbox_operation(<<"fetch-all">>) -> fetch_all;
%% inbox_operation(<<"fetch-new">>) -> fetch_new;
%% inbox_operation(<<"fetch-id">>)  -> fetch_id;
%% inbox_operation(<<"kill-all">>)  -> kill_all;
%% inbox_operation(<<"kill-old">>)  -> kill_old;
%% inbox_operation(<<"kill-id">>)   -> kill_id.

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
