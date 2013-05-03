-module(soap_srv_handlers).

-include("soap_srv_protocol.hrl").
-include("soap_srv.hrl").

-export([handle/1]).

-define(gv(K, PList), proplists:get_value(K, PList)).
-define(gv(K, PList, Default), proplists:get_value(K, PList, Default)).

handle(Req = #'SendSms'{}) ->
	User = Req#'SendSms'.user,
	Req2 = #send_req{
		action = 'SendSms',
		customer_id = User#user.'CustomerID',
		user_name = User#user.'Name',
		password = User#user.'Password',
		recipients = Req#'SendSms'.recipientPhone,
		originator = Req#'SendSms'.originator,
		text = Req#'SendSms'.smsText,
		type = Req#'SendSms'.messageType,
		def_date =  Req#'SendSms'.defDate,
		flash = Req#'SendSms'.flash
	},
	{ok, Result} = soap_mt_srv:send(Req2),
	send_result(Result);

handle(Req = #'HTTP_SendSms'{}) ->
	Req2 = #send_req{
		action = 'HTTP_SendSms',
		customer_id = Req#'HTTP_SendSms'.'customerID',
		user_name = Req#'HTTP_SendSms'.'userName',
		password = Req#'HTTP_SendSms'.'userPassword',
		recipients = Req#'HTTP_SendSms'.recipientPhone,
		originator = Req#'HTTP_SendSms'.originator,
		text = Req#'HTTP_SendSms'.smsText,
		type = Req#'HTTP_SendSms'.messageType,
		def_date =  Req#'HTTP_SendSms'.defDate,
		flash = Req#'HTTP_SendSms'.flash
	},
	{ok, Result} = soap_mt_srv:send(Req2),
	send_result(Result);

handle(Req = #'SendSms2'{}) ->
	User = Req#'SendSms2'.user,
	Req2 = #send_req{
		action = 'SendSms2',
		customer_id = User#user.'CustomerID',
		user_name = User#user.'Name',
		password = User#user.'Password',
		recipients = base64:decode(Req#'SendSms2'.recipientPhonesFile),
		originator = Req#'SendSms2'.originator,
		text = Req#'SendSms2'.smsText,
		type = Req#'SendSms2'.messageType,
		def_date =  Req#'SendSms2'.defDate,
		flash = Req#'SendSms2'.flash
	},
	{ok, Result} = soap_mt_srv:send(Req2),
	send_result(Result);

handle(Req = #'SendServiceSms'{}) ->
	Req2 = #send_req{
		action = 'SendServiceSms',
		customer_id = Req#'SendServiceSms'.'customerID',
		user_name = Req#'SendServiceSms'.userName,
		password = Req#'SendServiceSms'.userPassword,
		recipients = Req#'SendServiceSms'.recipientPhone,
		originator = Req#'SendServiceSms'.originator,
		s_name = Req#'SendServiceSms'.serviceName,
		s_url = Req#'SendServiceSms'.serviceUrl,
		type = Req#'SendServiceSms'.messageType,
		def_date =  Req#'SendServiceSms'.defDate,
		flash = Req#'SendServiceSms'.flash
	},
	{ok, Result} = soap_mt_srv:send(Req2),
	send_result(Result);

handle(Req = #'SendBinarySms'{}) ->
	User = Req#'SendBinarySms'.user,
	Req2 = #send_req{
		action = 'SendBinarySms',
		customer_id = User#user.'CustomerID',
		user_name = User#user.'Name',
		password = User#user.'Password',
		originator = Req#'SendBinarySms'.originator,
		binary_body = Req#'SendBinarySms'.binaryBody,
		recipients = Req#'SendBinarySms'.recipientPhone,
		def_date = Req#'SendBinarySms'.defDate,
		data_coding = Req#'SendBinarySms'.data_coding,
		esm_class = Req#'SendBinarySms'.esm_class,
		protocol_id = Req#'SendBinarySms'.'PID'
	},
	{ok, Result} = soap_mt_srv:send(Req2),
	send_result(Result);

handle(Req = #'HTTP_SendBinarySms'{}) ->
	Req2 = #send_req{
		action = 'HTTP_SendBinarySms',
		customer_id = Req#'HTTP_SendBinarySms'.'customerID',
		user_name = Req#'HTTP_SendBinarySms'.'userName',
		password = Req#'HTTP_SendBinarySms'.'userPassword',
		originator = Req#'HTTP_SendBinarySms'.originator,
		binary_body = Req#'HTTP_SendBinarySms'.binaryBody,
		recipients = Req#'HTTP_SendBinarySms'.recipientPhone,
		def_date = Req#'HTTP_SendBinarySms'.defDate,
		data_coding = Req#'HTTP_SendBinarySms'.data_coding,
		esm_class = Req#'HTTP_SendBinarySms'.esm_class,
		protocol_id = Req#'HTTP_SendBinarySms'.'PID'
	},
	{ok, Result} = soap_mt_srv:send(Req2),
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

handle(_) -> erlang:error(method_not_implemented).

%% ===================================================================
%% Internals
%% ===================================================================

send_result(Result) when is_list(Result) ->
	{ok, #'SendResult'{
			'Result' = ?gv(result, Result, <<"OK">>),
			'RejectedNumbers' = [Addr#addr.addr || Addr <- ?gv(rejected, Result, [])],
			'TransactionID' = ?gv(id, Result),
			'NetPoints' = <<"POSTPAID">> }}.

handle_authenticate(CustomerID, UserName, Password) ->
	case soap_auth_srv:authenticate(CustomerID, UserName, Password) of
		{ok, Customer} ->
			Originators =
				[Addr#addr.addr ||
					Addr <- Customer#k1api_auth_response_dto.allowed_sources],
			{ok, #'AuthResult'{
					'Result' = <<"OK">>,
					'NetPoints' = <<"POSTPAID">>,
					'Originators' = Originators,
					'CustomerID' = CustomerID,
					'CreditSMS' = <<"POSTPAID">>
					}};
		{error, timeout} ->
			{ok, #'AuthResult'{
					'Result' = ?authError}}
	end.

handle_keep_alive(CustomerID, UserName, Password) ->
	case soap_auth_srv:authenticate(CustomerID, UserName, Password) of
		{ok, _Customer} ->
			{ok, #'CommonResult'{'Result' = <<"OK">>}};
		{error, timeout} ->
			{ok, #'CommonResult'{'Result' = ?authError}}
	end.
