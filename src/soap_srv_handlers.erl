-module(soap_srv_handlers).

-include("soap_srv_protocol.hrl").
-include("soap_srv.hrl").

-export([handle/1]).


handle(Req = #'SendSms'{}) ->
	User = Req#'SendSms'.user,
	Recipients = Req#'SendSms'.recipientPhone,
	SendSmsReq = #send_sms_req{
		customer_id = User#user.'CustomerID',
		user_name = User#user.'Name',
		password = User#user.'Password',
		originator = soap_utils:addr_to_dto(Req#'SendSms'.originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = convert_numbers(Req#'SendSms'.smsText, Req#'SendSms'.'messageType'),
		type = latin,
		def_date = undefined,
		flash = get_boolean(Req#'SendSms'.flash)
	},
	{ok, RequestID, Rejected} = soap_mt_srv:process(SendSmsReq),
	{ok, #'SendResult'{
			'Result' = <<"OK">>,
			'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
			'TransactionID' = RequestID,
			'NetPoints' = <<"POSTPAID">> }};

handle(Req = #'HTTP_SendSms'{}) ->
	Recipients = Req#'HTTP_SendSms'.recipientPhone,
	SendSmsReq = #send_sms_req{
		customer_id = Req#'HTTP_SendSms'.'customerID',
		user_name = Req#'HTTP_SendSms'.userName,
		password = Req#'HTTP_SendSms'.userPassword,
		originator = soap_utils:addr_to_dto(Req#'HTTP_SendSms'.originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = convert_numbers(Req#'HTTP_SendSms'.smsText, Req#'HTTP_SendSms'.'messageType'),
		type = latin,
		def_date = undefined,
		flash = get_boolean(Req#'HTTP_SendSms'.flash)
	},
	{ok, RequestID, Rejected} = soap_mt_srv:process(SendSmsReq),
	{ok, #'SendResult'{
			'Result' = <<"OK">>,
			'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
			'TransactionID' = RequestID,
			'NetPoints' = <<"POSTPAID">> }};

handle(Req = #'SendSms2'{}) ->
	Recipients = base64:decode(Req#'SendSms2'.recipientPhonesFile),
	User = Req#'SendSms2'.user,
	SendSmsReq = #send_sms_req{
		customer_id = User#user.'CustomerID',
		user_name = User#user.'Name',
		password = User#user.'Password',
		originator = soap_utils:addr_to_dto(Req#'SendSms2'.originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		text = convert_numbers(Req#'SendSms2'.smsText, Req#'SendSms2'.'messageType'),
		type = latin,
		def_date = undefined,
		flash = get_boolean(Req#'SendSms2'.flash)
	},
	{ok, RequestID, Rejected} = soap_mt_srv:process(SendSmsReq),
	{ok, #'SendResult'{
			'Result' = <<"OK">>,
			'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
			'TransactionID' = RequestID,
			'NetPoints' = <<"POSTPAID">> }};

handle(Req = #'SendServiceSms'{}) ->
	Recipients = Req#'SendServiceSms'.recipientPhone,
	SendServiceSmsReq = #send_service_sms_req{
		customer_id = Req#'SendServiceSms'.'customerID',
		user_name = Req#'SendServiceSms'.userName,
		password = Req#'SendServiceSms'.userPassword,
		originator = soap_utils:addr_to_dto(Req#'SendServiceSms'.originator),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		service_name = Req#'SendServiceSms'.serviceName,
		service_url = Req#'SendServiceSms'.serviceUrl,
		type = latin,
		def_date = undefined,
		flash = get_boolean(Req#'SendServiceSms'.flash)
	},
	{ok, RequestID, Rejected} = soap_mt_srv:process(SendServiceSmsReq),
	{ok, #'SendResult'{
			'Result' = <<"OK">>,
			'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
			'TransactionID' = RequestID,
			'NetPoints' = <<"POSTPAID">> }};
handle(Req = #'SendBinarySms'{}) ->
	User = Req#'SendBinarySms'.user,
	Recipients = Req#'SendBinarySms'.recipientPhone,
	SendBinarySmsReq = #send_binary_sms_req{
		customer_id = User#user.'CustomerID',
		user_name = User#user.'Name',
		password = User#user.'Password',
		originator = soap_utils:addr_to_dto(Req#'SendBinarySms'.originator),
		binary_body = hexstr_to_bin(binary_to_list(Req#'SendBinarySms'.binaryBody)),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		def_date = undefined,
		data_coding = list_to_integer(binary_to_list(Req#'SendBinarySms'.data_coding)),
		esm_class = list_to_integer(binary_to_list(Req#'SendBinarySms'.esm_class)),
		protocol_id = list_to_integer(binary_to_list(Req#'SendBinarySms'.'PID'))
	},
	{ok, RequestID, Rejected} = soap_mt_srv:process(SendBinarySmsReq),
	{ok, #'SendResult'{
			'Result' = <<"OK">>,
			'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
			'TransactionID' = RequestID,
			'NetPoints' = <<"POSTPAID">> }};

handle(Req = #'HTTP_SendBinarySms'{}) ->
	Recipients = Req#'HTTP_SendBinarySms'.recipientPhone,
	SendBinarySmsReq = #send_binary_sms_req{
		customer_id = Req#'HTTP_SendBinarySms'.customerID,
		user_name = Req#'HTTP_SendBinarySms'.userName,
		password = Req#'HTTP_SendBinarySms'.userPassword,
		originator = soap_utils:addr_to_dto(Req#'HTTP_SendBinarySms'.originator),
		binary_body = hexstr_to_bin(binary_to_list(Req#'HTTP_SendBinarySms'.binaryBody)),
		recipients = [soap_utils:addr_to_dto(R) || R <- binary:split(Recipients, <<",">>, [trim, global])],
		def_date = undefined,
		data_coding = list_to_integer(binary_to_list(Req#'HTTP_SendBinarySms'.data_coding)),
		esm_class = list_to_integer(binary_to_list(Req#'HTTP_SendBinarySms'.esm_class)),
		protocol_id = list_to_integer(binary_to_list(Req#'HTTP_SendBinarySms'.'PID'))
	},
	{ok, RequestID, Rejected} = soap_mt_srv:process(SendBinarySmsReq),
	{ok, #'SendResult'{
			'Result' = <<"OK">>,
			'RejectedNumbers' = [Addr#addr.addr || Addr <- Rejected],
			'TransactionID' = RequestID,
			'NetPoints' = <<"POSTPAID">> }};

handle(Req = #'KeepAlive'{}) ->
	User = Req#'KeepAlive'.user,
	CustomerID = User#user.'CustomerID',
	UserName = User#user.'Name',
	Password = User#user.'Password',
	{ok, _Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Password),
	{ok, #'CommonResult'{
			'Result' = <<"OK">>}};

handle(Req = #'HTTP_KeepAlive'{}) ->
	CustomerID = Req#'HTTP_KeepAlive'.customerID,
	UserName = Req#'HTTP_KeepAlive'.userName,
	Password = Req#'HTTP_KeepAlive'.userPassword,
	{ok, _Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Password),
	{ok, #'CommonResult'{
			'Result' = <<"OK">>}};

handle(Req = #'HTTP_Authenticate'{}) ->
	CustomerID = Req#'HTTP_Authenticate'.customerID,
	UserName = Req#'HTTP_Authenticate'.userName,
	Password = Req#'HTTP_Authenticate'.userPassword,
	{ok, Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Password),
	Originators = [Addr#addr.addr || Addr <- Customer#k1api_auth_response_dto.allowed_sources],
	{ok, #'AuthResult'{
			'Result' = <<"OK">>,
			'NetPoints' = <<"POSTPAID">>,
			'Originators' = Originators,
			'CustomerID' = CustomerID,
			'CreditSMS' = <<"POSTPAID">>
			}};

handle(Req = #'Authenticate'{}) ->
	User = Req#'Authenticate'.user,
	CustomerID = User#user.'CustomerID',
	UserName = User#user.'Name',
	Password = User#user.'Password',
	{ok, Customer} =
		soap_auth_srv:authenticate(CustomerID, UserName, Password),
	Originators = [Addr#addr.addr || Addr <- Customer#k1api_auth_response_dto.allowed_sources],
	{ok, #'AuthResult'{
			'Result' = <<"OK">>,
			'NetPoints' = <<"POSTPAID">>,
			'Originators' = Originators,
			'CustomerID' = CustomerID,
			'CreditSMS' = <<"POSTPAID">>
			}};

handle(_) -> erlang:error(method_not_implemented).

%% ===================================================================
%% Internals
%% ===================================================================

get_boolean(<<"true">>) -> true;
get_boolean(<<"false">>) -> false.

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

convert_numbers(Text, <<"ArabicWithArabicNumbers">>) ->
	case unicode:characters_to_list(Text, utf8) of
		CodePoints when is_list(CodePoints) ->
			lager:info("Text ~w to ~w" , [Text, CodePoints]),
			ConvCP = [number_to_arabic(CP) || CP <- CodePoints],
			unicode:characters_to_binary(ConvCP, utf8);
		{error, CodePoints, RestData} ->
			lager:error("Arabic numbers to hindi error. Original: ~w Codepoints: ~w Rest: ~w",
					[Text, CodePoints, RestData]),
			erlang:error("Illegal utf8 symbols");
		{incomplete, CodePoints, IncompleteSeq} ->
			lager:error("Incomplete utf8 sequence. Original: ~w Codepoints: ~w IncompleteSeq: ~w",
					[Text, CodePoints, IncompleteSeq]),
			erlang:error("Incomplite utf8 sequence")
	end;
convert_numbers(Text, _) ->
	Text.

number_to_arabic(16#0030) -> 16#0660;
number_to_arabic(16#0031) -> 16#0661;
number_to_arabic(16#0032) -> 16#0662;
number_to_arabic(16#0033) -> 16#0663;
number_to_arabic(16#0034) -> 16#0664;
number_to_arabic(16#0035) -> 16#0665;
number_to_arabic(16#0036) -> 16#0666;
number_to_arabic(16#0037) -> 16#0667;
number_to_arabic(16#0038) -> 16#0668;
number_to_arabic(16#0039) -> 16#0669;
number_to_arabic(Any) -> Any.
