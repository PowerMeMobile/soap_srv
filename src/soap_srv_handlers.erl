-module(soap_srv_handlers).

-include("soap_srv_protocol.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("alley_services/include/alley_services.hrl").

-export([handle/1]).

-define(E_SUCCESS,
    <<"OK">>).
-define(E_AUTHENTICATION,
    <<"404.2 FAILURE (User is unknown)">>).
-define(E_INTERNAL,
    <<"500 FAILURE (Internal server error)">>).
-define(E_NOT_IMPLEMENTED,
    <<"501 FAILURE (Not implemented)">>).
-define(E_INVALID_RECIPIENTS,
    <<"400.1 FAILURE (Invalid recipients format)">>).
-define(E_TIMEOUT,
    <<"504 FAILURE (Request timeout)">>).
-define(E_ORIGINATOR_NOT_FOUND,
    <<"600.1 Originator for customerID is not found">>).
-define(E_NO_RECIPIENTS,
    <<"600.4 Phone not specified">>).
-define(E_NO_DEST_ADDRS,
    <<"FAILURE: All recipient numbers in your message "
      "are either Rejected or Blacklisted">>).
-define(E_INVALID_DEF_DATE,
    <<"Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS">>).
-define(E_NO_MESSAGE_BODY,
    <<"Message Content Is Empty">>).
-define(E_INVALID_REQUEST_ID,
    <<"SMS ID for status request is incorrect or not specified">>).
-define(E_EMPTY_REQUEST_ID,
    <<"605.7 The action you requested cannot be performed, "
      "because one of your the required request parameters "
      "('TransactionID') was not supplied.">>).
-define(E_SERVICE_NAME_OR_URL_EXPECTED,
     <<"Service name and url is expected">>).
-define(E_CREDIT_LIMIT_EXCEEDED,
    <<"Customer's postpaid credit limit is exceeded">>).
-define(E_INBOX_BAD_OPERATION,
    <<"Non-supported Inbox operation is specified!">>).
-define(E_INBOX_NOT_ACTIVATED,
    <<"Inbox is not activated">>).
-define(E_INVALID_ENCODING,
    <<"Invalid encoding">>).

%% ===================================================================
%% API
%% ===================================================================

-spec handle(tuple()) -> {ok, tuple()}.
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
                Addr <- Customer#auth_customer_v2.allowed_sources],
            Credit = credit_left(Customer#auth_customer_v2.pay_type,
                                 Customer#auth_customer_v2.credit),
            {ok, #'AuthResult'{
                'Result' = ?E_SUCCESS,
                'NetPoints' = Credit,
                'Originators' = Originators,
                'CustomerID' = CustomerID,
                'CreditSMS' = Credit
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
                Addr <- Customer#auth_customer_v2.allowed_sources],
            Credit = credit_left(Customer#auth_customer_v2.pay_type,
                                 Customer#auth_customer_v2.credit),
            {ok, #'AuthResult'{
                'Result' = ?E_SUCCESS,
                'NetPoints' = Credit,
                'Originators' = Originators,
                'CustomerID' = CustomerID,
                'CreditSMS' = Credit
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
            handle(check_inbox_activated, Req, Customer);
        {error, Error} ->
            Operation = Req#'InboxProcessing'.operation,
            handle_inbox_error_response(Operation, Error)
    end;

handle(authenticate, Req = #'HTTP_InboxProcessing'{}) ->
    CustomerId = Req#'HTTP_InboxProcessing'.customerID,
    UserId = Req#'HTTP_InboxProcessing'.userName,
    Password = Req#'HTTP_InboxProcessing'.userPassword,
    case authenticate(CustomerId, UserId, Password) of
        {ok, Customer} ->
            handle(check_inbox_activated, Req, Customer);
        {error, Error} ->
            Operation = Req#'HTTP_InboxProcessing'.operation,
            handle_inbox_error_response(Operation, Error)
    end;

handle(_, _) ->
    erlang:error(method_not_implemented).

handle(check_params, Req = #'SendSms'{}, Customer) ->
    SmsText = Req#'SendSms'.smsText,
    case SmsText of
        <<>> ->
            send_result(#send_result{result = no_message_body});
        undefined ->
            send_result(#send_result{result = no_message_body});
        _ ->
            DefDate = Req#'SendSms'.defDate,
            case parse_def_date(DefDate) of
                {ok, ParsedDefDate} ->
                    Req2 = Req#'SendSms'{defDate = ParsedDefDate},
                    handle(check_encoding, Req2, Customer);
                {error, invalid} ->
                    send_result(#send_result{result = invalid_def_date})
            end
    end;

handle(check_params, Req = #'HTTP_SendSms'{}, Customer) ->
    SmsText =  Req#'HTTP_SendSms'.smsText,
    case SmsText of
        <<>> ->
            send_result(#send_result{result = no_message_body});
        undefined ->
            send_result(#send_result{result = no_message_body});
        _ ->
            DefDate = Req#'HTTP_SendSms'.defDate,
            case parse_def_date(DefDate) of
                {ok, ParsedDefDate} ->
                    Req2 = Req#'HTTP_SendSms'{defDate = ParsedDefDate},
                    handle(check_encoding, Req2, Customer);
                {error, invalid} ->
                    send_result(#send_result{result = invalid_def_date})
            end
    end;

handle(check_params, Req = #'SendSms2'{}, Customer) ->
    try base64:decode(Req#'SendSms2'.recipientPhonesFile) of
        Recipients ->
            Req2 = Req#'SendSms2'{recipientPhonesFile = Recipients},
            SmsText = Req#'SendSms2'.smsText,
            case SmsText of
                <<>> ->
                    send_result(#send_result{result = no_message_body});
                undefined ->
                    send_result(#send_result{result = no_message_body});
                _ ->
                    DefDate = Req2#'SendSms2'.defDate,
                    case parse_def_date(DefDate) of
                        {ok, ParsedDefDate} ->
                            Req3 = Req2#'SendSms2'{defDate = ParsedDefDate},
                            handle(check_encoding, Req3, Customer);
                        {error, invalid} ->
                            send_result(#send_result{result = invalid_def_date})
                    end
            end
    catch
        _:_ ->
            ?log_error("Invalid recipientPhonesFile: ~p",
                [Req#'SendSms2'.recipientPhonesFile]),
            send_result(#send_result{result = no_recipients})
    end;

handle(check_params, Req = #'SendServiceSms'{}, Customer) ->
    Name = Req#'SendServiceSms'.serviceName,
    Url = Req#'SendServiceSms'.serviceUrl,
    case is_binary(Name) andalso is_binary(Url) of
        true ->
            DefDate = Req#'SendServiceSms'.defDate,
            case parse_def_date(DefDate) of
                {ok, ParsedDefDate} ->
                    Req2 = Req#'SendServiceSms'{defDate = ParsedDefDate},
                    handle(check_encoding, Req2, Customer);
                {error, invalid} ->
                    send_result(#send_result{result = invalid_def_date})
            end;
        false ->
            send_result(#send_result{result = bad_service_name_or_url})
    end;

handle(check_params, Req = #'SendBinarySms'{}, Customer) ->
    BinaryBody = Req#'SendBinarySms'.binaryBody,
    case BinaryBody of
        <<>> ->
            send_result(#send_result{result = no_message_body});
        undefined ->
            send_result(#send_result{result = no_message_body});
        _ ->
            DefDate = Req#'SendBinarySms'.defDate,
            case parse_def_date(DefDate) of
                {ok, ParsedDefDate} ->
                    Req2 = Req#'SendBinarySms'{defDate = ParsedDefDate},
                    handle(send, Req2, Customer);
                {error, invalid} ->
                    send_result(#send_result{result = invalid_def_date})
        end
    end;

handle(check_params, Req = #'HTTP_SendBinarySms'{}, Customer) ->
    BinaryBody = Req#'HTTP_SendBinarySms'.binaryBody,
    case BinaryBody of
        <<>> ->
            send_result(#send_result{result = no_message_body});
        undefined ->
            send_result(#send_result{result = no_message_body});
        _ ->
            DefDate = Req#'HTTP_SendBinarySms'.defDate,
            case parse_def_date(DefDate) of
                {ok, ParsedDefDate} ->
                    Req2 = Req#'HTTP_SendBinarySms'{defDate = ParsedDefDate},
                    handle(send, Req2, Customer);
                {error, invalid} ->
                    send_result(#send_result{result = invalid_def_date})
        end
    end;

handle(check_encoding, Req = #'SendSms'{}, Customer) ->
    SmsText = Req#'SendSms'.smsText,
    MsgType = Req#'SendSms'.messageType,
    case check_encoding(SmsText, MsgType) of
        ok ->
            handle(send, Req, Customer);
        {error, {given, Given, guessed, Guessed}} ->
            ?log_error("Invalid encoding detected text: ~p, given: ~p, guessed: ~p",
                [SmsText, Given, Guessed]),
            send_result(#send_result{result = invalid_encoding})
    end;

handle(check_encoding, Req = #'HTTP_SendSms'{}, Customer) ->
    SmsText = Req#'HTTP_SendSms'.smsText,
    MsgType = Req#'HTTP_SendSms'.messageType,
    case check_encoding(SmsText, MsgType) of
        ok ->
            handle(send, Req, Customer);
        {error, {given, Given, guessed, Guessed}} ->
            ?log_error("Invalid encoding detected text: ~p, given: ~p, guessed: ~p",
                [SmsText, Given, Guessed]),
            send_result(#send_result{result = invalid_encoding})
    end;

handle(check_encoding, Req = #'SendSms2'{}, Customer) ->
    SmsText = Req#'SendSms2'.smsText,
    MsgType = Req#'SendSms2'.messageType,
    case check_encoding(SmsText, MsgType) of
        ok ->
            handle(send, Req, Customer);
        {error, {given, Given, guessed, Guessed}} ->
            ?log_error("Invalid encoding detected text: ~p, given: ~p, guessed: ~p",
                [SmsText, Given, Guessed]),
            send_result(#send_result{result = invalid_encoding})
    end;

handle(check_encoding, Req = #'SendServiceSms'{}, Customer) ->
    SmsText = reformat_service_sms(
        Req#'SendServiceSms'.serviceName, Req#'SendServiceSms'.serviceUrl),
    MsgType = Req#'SendServiceSms'.messageType,
    case check_encoding(SmsText, MsgType) of
        ok ->
            handle(send, Req, Customer);
        {error, {given, Given, guessed, Guessed}} ->
            ?log_error("Invalid encoding detected text: ~p, given: ~p, guessed: ~p",
                [SmsText, Given, Guessed]),
            send_result(#send_result{result = invalid_encoding})
    end;

handle(send, Req = #'SendSms'{user = User}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = User#user.'Name',
    {Encoding, NumType} = reformat_message_type(Req#'SendSms'.messageType),
    Message = alley_services_utils:convert_arabic_numbers(Req#'SendSms'.smsText, NumType),
    Size = alley_services_utils:chars_size(Encoding, Message),
    Flash = reformat_boolean(Req#'SendSms'.flash),
    Params = flash(Flash, Encoding) ++ common_smpp_params(Customer) ++ [
        {esm_class, 3},
        {protocol_id, 0}
    ],
    Req2 = #send_req{
        customer = Customer,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        interface = soap,
        originator = reformat_addr(Req#'SendSms'.originator),
        recipients = reformat_addrs(Req#'SendSms'.recipientPhone),

        req_type = single,
        message = Message,
        encoding = Encoding,
        size = Size,
        params = Params,

        def_time = Req#'SendSms'.defDate
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            send_result(#send_result{result = Error})
        end;

handle(send, Req = #'HTTP_SendSms'{}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = Req#'HTTP_SendSms'.'userName',
    {Encoding, NumType} = reformat_message_type(Req#'HTTP_SendSms'.messageType),
    Message = alley_services_utils:convert_arabic_numbers(Req#'HTTP_SendSms'.smsText, NumType),
    Size = alley_services_utils:chars_size(Encoding, Message),
    Flash = reformat_boolean(Req#'HTTP_SendSms'.flash),
    Params = flash(Flash, Encoding) ++ common_smpp_params(Customer) ++ [
        {esm_class, 3},
        {protocol_id, 0}
    ],
    Req2 = #send_req{
        customer = Customer,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        interface = soap,
        originator = reformat_addr(Req#'HTTP_SendSms'.originator),
        recipients = reformat_addrs(Req#'HTTP_SendSms'.recipientPhone),

        req_type = single,
        message = Message,
        encoding = Encoding,
        size = Size,
        params = Params,

        def_time = Req#'HTTP_SendSms'.defDate
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            send_result(#send_result{result = Error})
    end;

handle(send, Req = #'SendSms2'{user = User}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = User#user.'Name',
    {Encoding, NumType} = reformat_message_type(Req#'SendSms2'.messageType),
    Message = alley_services_utils:convert_arabic_numbers(Req#'SendSms2'.smsText, NumType),
    Size = alley_services_utils:chars_size(Encoding, Message),
    Flash = reformat_boolean(Req#'SendSms2'.flash),
    Params = flash(Flash, Encoding) ++ common_smpp_params(Customer) ++ [
        {esm_class, 3},
        {protocol_id, 0}
    ],
    Req2 = #send_req{
        customer = Customer,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        interface = soap,
        originator = reformat_addr(Req#'SendSms2'.originator),
        recipients = reformat_addrs(Req#'SendSms2'.recipientPhonesFile),

        req_type = single,
        message = Message,
        encoding = Encoding,
        size = Size,
        params = Params,

        def_time = Req#'SendSms2'.defDate
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            send_result(#send_result{result = Error})
    end;

handle(send, Req = #'SendServiceSms'{}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = Req#'SendServiceSms'.userName,
    {Encoding, _NumType} = reformat_message_type(Req#'SendServiceSms'.messageType),
    Message = reformat_service_sms(
        Req#'SendServiceSms'.serviceName, Req#'SendServiceSms'.serviceUrl),
    Size = alley_services_utils:chars_size(Encoding, Message),
    Params = common_smpp_params(Customer) ++ [
        {esm_class, 64},
        {protocol_id, 0},
        {data_coding, 245},
        {source_port, 9200},
        {destination_port, 2948}
    ],
    Req2 = #send_req{
        customer = Customer,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        interface = soap,
        originator = reformat_addr(Req#'SendServiceSms'.originator),
        recipients = reformat_addrs(Req#'SendServiceSms'.recipientPhone),

        req_type = single,
        message = Message,
        encoding = Encoding,
        size = Size,
        params = Params,

        def_time = Req#'SendServiceSms'.defDate
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            send_result(#send_result{result = Error})
    end;

handle(send, Req = #'SendBinarySms'{user = User}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = User#user.'Name',
    Message = ac_hexdump:hexdump_to_binary(Req#'SendBinarySms'.binaryBody),
    DC = reformat_integer(Req#'SendBinarySms'.data_coding),
    ESMClass = reformat_integer(Req#'SendBinarySms'.esm_class),
    ProtocolId = reformat_integer(Req#'SendBinarySms'.'PID'),
    Params = common_smpp_params(Customer) ++ [
        {data_coding, DC},
        {esm_class, ESMClass},
        {protocol_id, ProtocolId}
    ],
    Req2 = #send_req{
        customer = Customer,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        interface = soap,
        originator = reformat_addr(Req#'SendBinarySms'.originator),
        recipients = reformat_addrs(Req#'SendBinarySms'.recipientPhone),

        req_type = single,
        message = Message,
        encoding = default,
        size = size(Message),
        params = Params,

        def_time = Req#'SendBinarySms'.defDate
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            send_result(#send_result{result = Error})
    end;

handle(send, Req = #'HTTP_SendBinarySms'{}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = Req#'HTTP_SendBinarySms'.'userName',
    Message = ac_hexdump:hexdump_to_binary(Req#'HTTP_SendBinarySms'.binaryBody),
    DC = reformat_integer(Req#'HTTP_SendBinarySms'.data_coding),
    ESMClass = reformat_integer(Req#'HTTP_SendBinarySms'.esm_class),
    ProtocolId = reformat_integer(Req#'HTTP_SendBinarySms'.'PID'),
    Params = common_smpp_params(Customer) ++ [
        {data_coding, DC},
        {esm_class, ESMClass},
        {protocol_id, ProtocolId}
    ],
    Req2 = #send_req{
        customer = Customer,
        customer_uuid = CustomerUuid,
        user_id = UserId,
        interface = soap,
        originator = reformat_addr(Req#'HTTP_SendBinarySms'.originator),
        recipients = reformat_addrs(Req#'HTTP_SendBinarySms'.recipientPhone),

        req_type = single,
        message = Message,
        encoding = default,
        size = size(Message),
        params = Params,

        def_time = Req#'HTTP_SendBinarySms'.defDate
    },
    case alley_services_mt:send(Req2) of
        {ok, Result} ->
            ?log_debug("Got submit result: ~p", [Result]),
            send_result(Result);
        {error, Error} ->
            ?log_error("SendSms failed with: ~p", [Error]),
            send_result(#send_result{result = Error})
    end;

handle(get_sms_status, Req = #'GetSmsStatus'{user = User}, Customer) ->
    UserId = User#user.'Name',
    TransactionId = Req#'GetSmsStatus'.transactionID,
    Detailed = reformat_boolean(Req#'GetSmsStatus'.detailed),
    get_sms_status(Customer, UserId, TransactionId, Detailed);

handle(get_sms_status, Req = #'HTTP_GetSmsStatus'{}, Customer) ->
    UserId = Req#'HTTP_GetSmsStatus'.userName,
    TransactionId = Req#'HTTP_GetSmsStatus'.transactionID,
    Detailed = reformat_boolean(Req#'HTTP_GetSmsStatus'.detailed),
    get_sms_status(Customer, UserId, TransactionId, Detailed);

handle(check_inbox_activated, Req = #'InboxProcessing'{}, Customer) ->
    case lists:keyfind(<<"inbox">>, #feature_v1.name, Customer#auth_customer_v2.features) of
        #feature_v1{value = <<"true">>} ->
            handle(inbox_processing, Req, Customer);
        _Other ->
            Operation = Req#'InboxProcessing'.operation,
            handle_inbox_error_response(Operation, inbox_not_activated)
    end;

handle(check_inbox_activated, Req = #'HTTP_InboxProcessing'{}, Customer) ->
    case lists:keyfind(<<"inbox">>, #feature_v1.name, Customer#auth_customer_v2.features) of
        #feature_v1{value = <<"true">>} ->
            handle(inbox_processing, Req, Customer);
        _Other ->
            Operation = Req#'HTTP_InboxProcessing'.operation,
            handle_inbox_error_response(Operation, inbox_not_activated)
    end;

handle(inbox_processing, Req = #'InboxProcessing'{user = User}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = User#user.'Name',
    Operation = inbox_operation(Req#'InboxProcessing'.operation),
    MsgIds = inbox_msg_ids(Req#'InboxProcessing'.messageId),
    Credit = credit_left(Customer#auth_customer_v2.pay_type,
                         Customer#auth_customer_v2.credit),
    inbox_processing(CustomerUuid, UserId, Operation, MsgIds, Credit);

handle(inbox_processing, Req = #'HTTP_InboxProcessing'{}, Customer) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    UserId = Req#'HTTP_InboxProcessing'.userName,
    Operation = inbox_operation(Req#'HTTP_InboxProcessing'.operation),
    MsgIds = inbox_msg_ids(Req#'HTTP_InboxProcessing'.messageId),
    Credit = credit_left(Customer#auth_customer_v2.pay_type,
                         Customer#auth_customer_v2.credit),
    inbox_processing(CustomerUuid, UserId, Operation, MsgIds, Credit);

handle(Method, Req, _Customer) ->
    ?log_error("Method not implemented method: ~p, req: ~p",
        [Method, Req]),
    erlang:error(method_not_implemented).

%% ===================================================================
%% Internal Handlers
%% ===================================================================

send_result(#send_result{
    result = ok,
    req_id = ReqId,
    rejected = Rejected,
    customer = Customer,
    credit_left = CreditLeft
}) ->
    {ok, #'SendResult'{
        'Result' = ?E_SUCCESS,
        'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
        'TransactionID' = ReqId,
        'NetPoints' = credit_left(Customer#auth_customer_v2.pay_type, CreditLeft)
    }};
send_result(#send_result{result = Result}) ->
    {ok, #'SendResult'{
        'Result' = reformat_error(Result)
    }}.

credit_left(postpaid, _) ->
    <<"POSTPAID">>;
credit_left(prepaid, undefined) ->
    <<"0">>;
credit_left(prepaid, Credit) when is_float(Credit) ->
    integer_to_binary(round(Credit)).

authenticate(CustomerID, UserName, Password) ->
    case alley_services_auth:authenticate(
            CustomerID, UserName, Password, soap) of
        {ok, #auth_resp_v2{result = Result}} ->
            case Result of
                #auth_customer_v2{} ->
                    {ok, Result};
                #auth_error_v2{code = Error} ->
                    ?log_error("Authenticate response error: ~p", [Error]),
                    {error, authentication}
            end;
        {error, Error} ->
            ?log_error("Authenticate failed with: ~p", [Error]),
            {error, Error}
    end.

get_sms_status(Customer, UserId, TransactionId, Detailed) ->
    CustomerUuid = Customer#auth_customer_v2.customer_uuid,
    case alley_services_api:get_sms_status(
            CustomerUuid, UserId, TransactionId) of
        {ok, #sms_status_resp_v1{statuses = Statuses}} ->
            Statistics = build_statistics(Statuses),
            Credit = credit_left(Customer#auth_customer_v2.pay_type,
                                 Customer#auth_customer_v2.credit),
            case Detailed of
                true ->
                    Details = build_details(Statuses),
                    {ok, #'SmsStatus'{
                        'Result' = ?E_SUCCESS,
                        'Statistics' = Statistics,
                        'Details' = Details,
                        'NetPoints' = Credit
                    }};
                false ->
                    {ok, #'SmsStatus'{
                        'Result' = ?E_SUCCESS,
                        'Statistics' = Statistics,
                        'NetPoints' = Credit
                    }}
            end;
        {error, Error} ->
            ?log_error("GetSmsStatus failed with: ~p", [Error]),
            {ok, #'SmsStatus'{'Result' = reformat_error(Error)}}
    end.

inbox_processing(CustomerUuid, UserId, Operation, MsgIds, Credit) ->
    case alley_services_api:process_inbox(CustomerUuid, UserId,
            Operation, MsgIds) of
        {ok, #inbox_resp_v1{result = Result}} ->
            handle_inbox_response(Result, Credit);
        {error, Error} ->
            handle_inbox_error_response(Operation, Error)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

handle_inbox_response({info, Info}, Credit) ->
    New   = Info#inbox_info_v1.new,
    Total = Info#inbox_info_v1.total,
    Result =
    <<
    "<inboxinfo xmlns=\"\">",
    "<result>OK</result>",
    "<credits>", Credit/binary, "</credits>",
    "<new>", (integer_to_binary(New))/binary, "</new>"
    "<total>", (integer_to_binary(Total))/binary, "</total>",
    "</inboxinfo>"
    >>,
    {ok, #'InlineResult'{'InlineBody' = Result}};

handle_inbox_response({messages, Messages}, Credit) ->
    Result =
    <<
    "<inboxlist xmlns=\"\">",
    "<result>OK</result>",
    "<credits>", Credit/binary, "</credits>",
    (list_to_binary(
        [build_inbox_message(M) || M <- Messages]))/binary,
    "</inboxlist>"
    >>,
    {ok, #'InlineResult'{'InlineBody' = Result}};

handle_inbox_response({deleted, Deleted}, Credit) ->
    Result =
    <<
    "<inboxdel xmlns=\"\">",
    "<result>OK</result>"
    "<credits>", Credit/binary, "</credits>",
    "<deleted>", (integer_to_binary(Deleted))/binary, "</deleted>",
    "</inboxdel>"
    >>,
    {ok, #'InlineResult'{'InlineBody' = Result}}.

handle_inbox_error_response(_Oper, bad_operation) ->
    Result =
    <<
    "<inbox xmlns=\"\">",
    "<result>", (reformat_error(bad_operation))/binary, "</result>",
    "</inbox>"
    >>,
    {ok, #'InlineResult'{'InlineBody' = Result}};
handle_inbox_error_response(Oper, Error) ->
    Tag =
        case inbox_operation(Oper) of
            get_info ->
                <<"inboxinfo">>;
            Op when Op =:= list_all; Op =:= list_new;
                    Op =:= fetch_all; Op =:= fetch_new; Op =:= fetch_id ->
                <<"inboxlist">>;
            Op when Op =:= delete_all; Op =:= delete_read; Op =:= delete_id ->
                <<"inboxdel">>;
            Other ->
                handle_inbox_error_response(Other, bad_operation)
        end,
    Result =
    <<
    "<", Tag/binary, " xmlns=\"\">",
    "<result>", (reformat_error(Error))/binary, "</result>",
    "</", Tag/binary, ">"
    >>,
    {ok, #'InlineResult'{'InlineBody' = Result}}.

inbox_operation(Oper) ->
    case bstr:lower(Oper) of
        <<"stats">>     -> get_info;
        <<"list-all">>  -> list_all;
        <<"list-new">>  -> list_new;
        <<"fetch-all">> -> fetch_all;
        <<"fetch-new">> -> fetch_new;
        <<"fetch-id">>  -> fetch_id;
        <<"kill-all">>  -> delete_all;
        <<"kill-old">>  -> delete_read;
        <<"kill-id">>   -> delete_id;
        Other -> Other
    end.

inbox_msg_ids(<<>>) ->
    undefined;
inbox_msg_ids(undefined) ->
    undefined;
inbox_msg_ids(MsgIds) ->
    binary:split(MsgIds, <<",">>, [trim, global]).

build_inbox_message(Msg) ->
    MsgId = Msg#inbox_msg_info_v1.msg_id,
    New = if Msg#inbox_msg_info_v1.state =:= new -> 1; true -> 0 end,
    From = Msg#inbox_msg_info_v1.src_addr#addr.addr,
    To = Msg#inbox_msg_info_v1.dst_addr#addr.addr,
    Timestamp = utc_timestamp_to_binary(Msg#inbox_msg_info_v1.rcv_time),
    Size = Msg#inbox_msg_info_v1.size,
    TextUtf8 = Msg#inbox_msg_info_v1.body,
    TextUtf16 = ac_hexdump:binary_to_hexdump(
        unicode:characters_to_binary(TextUtf8, utf8, {utf16, big}), to_lower),
    <<
    "<message id=\"", MsgId/binary, "\" new=\"", (integer_to_binary(New))/binary, "\">",
    "<from>", From/binary, "</from>",
    "<to>", To/binary, "</to>",
    "<timestamp>", Timestamp/binary, "</timestamp>",
    "<size>", (integer_to_binary(Size))/binary, "</size>",
    "<msgtype>SMS</msgtype>",
    "<textU>", TextUtf16/binary, "</textU>",
    "<subject/>",
    "</message>"
    >>.

utc_timestamp_to_binary(Timestamp) ->
    <<"20", (ac_datetime:datetime_to_utc_string(
                 calendar:universal_time_to_local_time(
                     ac_datetime:timestamp_to_datetime(Timestamp))))/binary>>.

build_details(Statuses) ->
    <<
    "<details xmlns=\"\">",
    (list_to_binary([detailed_status_tag(Status) || Status <- Statuses]))/binary,
    "</details>"
    >>.

detailed_status_tag(Status = #sms_status_v1{}) ->
    StatusName = Status#sms_status_v1.status,
    Number = Status#sms_status_v1.address,
    Timestamp = Status#sms_status_v1.timestamp,
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
    (list_to_binary(
        [status_tag(Status, Counter) || {Status, Counter} <- Agregated]))/binary,
    "</statistics>"
    >>.

status_tag(Status, Counter) ->
    Content =
    <<
    (integer_to_binary(Counter))/binary,
    (content_tag('StatusU', to_utf16_hexdump(Status)))/binary
    >>,
    content_tag(reformat_status_name(Status), Content).

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
aggregate_statistics([#sms_status_v1{status = Status} | Rest], Dict) ->
    Dict1 = dict:update_counter(Status, 1, Dict),
    aggregate_statistics(Rest, Dict1).

reformat_boolean(Value) when is_binary(Value) ->
    case bstr:lower(Value) of
        <<"true">>  -> true;
        <<"false">> -> false;
        _           -> false
    end;
reformat_boolean(undefined) -> false.

reformat_integer(undefined) ->
    0;
reformat_integer(<<>>) ->
    0;
reformat_integer(Binary) ->
    binary_to_integer(Binary).

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
reformat_error(bad_operation) ->
    ?E_INBOX_BAD_OPERATION;
reformat_error(inbox_not_activated) ->
    ?E_INBOX_NOT_ACTIVATED;
reformat_error(invalid_encoding) ->
    ?E_INVALID_ENCODING;
reformat_error(Result) ->
    atom_to_binary(Result, utf8).

parse_def_date(undefined) ->
    {ok, undefined};
parse_def_date(<<>>) ->
    {ok, undefined};
parse_def_date(Date) ->
    try
        %% YYYYMMDDHHMMSS in UTC
        {YearB, Date2} = split_binary(Date, 4),
        {MonB, Date3} = split_binary(Date2, 2),
        {DayB, Date4} = split_binary(Date3, 2),
        {HourB, Date5} = split_binary(Date4, 2),
        {MinB, Date6} = split_binary(Date5, 2),
        {SecB, _} = split_binary(Date6, 2),
        Year = binary_to_integer(YearB),
        Mon = binary_to_integer(MonB),
        Day = binary_to_integer(DayB),
        Hour = binary_to_integer(HourB),
        Min = binary_to_integer(MinB),
        Sec = binary_to_integer(SecB),
        DateTime = {{Year, Mon, Day}, {Hour, Min, Sec}},
        DefDate = ac_datetime:datetime_to_timestamp(DateTime),
        {ok, DefDate}
    catch
        _:_ ->
            {error, invalid}
    end.

reformat_message_type(<<"Latin">>) ->
    {default, to_latin};
reformat_message_type(<<"ArabicWithArabicNumbers">>) ->
    {ucs2, to_arabic};
reformat_message_type(<<"ArabicWithLatinNumbers">>) ->
    {ucs2, to_latin}.

reformat_service_sms(Name, Url) ->
    <<"<%SERVICEMESSAGE:", Name/binary, ";", Url/binary, "%>">>.

flash(false, _) ->
    [];
flash(true, default) ->
    [{data_coding, 240}];
flash(true, ucs2) ->
    [{data_coding, 248}].

common_smpp_params(Customer) ->
    ReceiptsAllowed = Customer#auth_customer_v2.receipts_allowed,
    NoRetry = Customer#auth_customer_v2.no_retry,
    Validity = alley_services_utils:fmt_validity(
        Customer#auth_customer_v2.default_validity),
    [
        {registered_delivery, ReceiptsAllowed},
        {service_type, <<>>},
        {no_retry, NoRetry},
        {validity_period, Validity},
        {priority_flag, 0}
    ].

check_encoding(Text, MsgType) ->
    {Encoding, _NumType} = reformat_message_type(MsgType),
    case alley_services_utils:guess_encoding(Text) of
        {ok, Encoding} ->
            ok;
        {ok, GuessedEnc} ->
            {error, {given, Encoding, guessed, GuessedEnc}};
        {error, unknown} ->
            {error, {given, Encoding, guessed, unknown}}
    end.
