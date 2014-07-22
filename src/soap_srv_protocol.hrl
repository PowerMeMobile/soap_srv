-ifndef(soap_srv_protocol_hrl).
-define(soap_srv_protocol_hrl, defined).

-include_lib("alley_dto/include/adto.hrl").

%% SOAP methods

-record(user, {
    'CustomerID' :: integer(),
    'Name' :: binary(),
    'Language' :: binary(),
    'Password' :: binary()
}).
-type user() :: #user{}.

-type messageType() ::
    'Latin' |
    'ArabicWithArabicNumbers' |
    'ArabicWithLatinNumbers'.

-record('SendSms', {
    'user' :: user(),
    'originator' :: binary(),
    'smsText' :: binary(),
    'recipientPhone' :: binary(),
    'messageType' :: messageType(),
    'defDate' :: binary(),
    'blink' :: boolean(),
    'flash' :: boolean(),
    'Private' :: boolean()
}).

-record('SendSms2', {
    'user' :: user(),
    'originator' :: binary(),
    'smsText' :: binary(),
    'recipientPhonesFile' :: binary(),
    'messageType' :: messageType(),
    'defDate' :: binary(),
    'flash' :: boolean()
}).

-record('SendServiceSms', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary(),
    'originator' :: binary(),
    'serviceName' :: binary(),
    'serviceUrl' :: binary(),
    'recipientPhone' :: binary(),
    'messageType' :: messageType(),
    'defDate' :: binary(),
    'blink' :: boolean(),
    'flash' :: boolean(),
    'Private' :: boolean()
}).

-record('SendBinarySms', {
    'user' :: user(),
    'originator' :: binary(),
    'binaryBody' :: binary(),
    'recipientPhone' :: binary(),
    'defDate' :: binary(),
    'data_coding' :: binary(),
    'esm_class' :: binary(),
    'PID' :: binary()
}).

-record('KeepAlive', {
    'user' :: user()
}).

-record('CommonResult', {
    'Result' :: binary()
}).

-record('HTTP_SendSms', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary(),
    'originator' :: binary(),
    'smsText' :: binary(),
    'recipientPhone' :: binary(),
    'messageType' :: messageType(),
    'defDate' :: binary(),
    'blink' :: boolean(),
    'flash' :: boolean(),
    'Private' :: boolean()
}).

-record('SendResult', {
    'Result' :: binary(),
    'RejectedNumbers' :: [binary()],
    'TransactionID' :: binary(),
    'NetPoints' = <<"0">> :: binary()
}).

-record('HTTP_SendBinarySms', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary(),
    'originator' :: binary(),
    'binaryBody' :: binary(),
    'recipientPhone' :: binary(),
    'defDate' :: binary(),
    'data_coding' :: binary(),
    'esm_class' :: binary(),
    'PID' :: binary()
}).

-record('HTTP_KeepAlive', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary()
}).

-record('HTTP_Authenticate', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary()
}).

-record('HTTP_GetSmsStatus', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary(),
    'transactionID' :: binary(),
    'detailed' :: boolean()
}).

-record('GetSmsStatus', {
    'user' :: user(),
    'transactionID' :: binary(),
    'detailed' :: boolean()
}).

-record('SmsStatus', {
    'Result' :: binary(),
    'Statistics' :: binary(),
    'Details' :: binary(),
    'NetPoints' :: binary()
}).

-record('HTTP_InboxProcessing', {
    'customerID' :: integer(),
    'userName' :: binary(),
    'userPassword' :: binary(),
    'operation' :: binary(),
    'messageId' :: binary()
}).

-record('InboxProcessing', {
    'user' :: user(),
    'operation' :: binary(),
    'messageId' :: binary()
}).

-record('Authenticate', {
    user :: #user{}
}).

-record('AuthResult', {
    'Result' :: binary(),
    'NetPoints' = <<"0">> :: binary(),
    'Originators' = [] :: [binary()],
    'CustomerID' = -1 :: integer(),
    'CreditSMS' :: binary(),
    'CreditMMS' :: binary()
}).

-endif. % soap_srv_protocol.hrl
