-ifndef(soap_srv_hrl).
-define(soap_srv_hrl, defined).

-include_lib("alley_dto/include/adto.hrl").

-type auth_resp() :: #k1api_auth_response_dto{}.
-record(send_req, {
    action          :: atom(),
    customer    :: undefined | auth_resp(),
    customer_id :: undefined | binary(),
    user_name   :: undefined | binary(),
    password    :: undefined | binary(),
    originator      :: undefined | binary(),
    recipients  :: undefined | binary(),
    text        :: undefined | binary(),
    type        :: undefined | binary(),
    def_date    :: undefined | binary(),
    flash       :: undefined | binary(),
    smpp_params :: undefined | term(),
    encoding    :: undefined | default | ucs2,
    encoded     :: undefined | binary(),
    rejected    :: undefined | [binary()],

    %% SendServiceSms extention
    s_name          :: undefined | binary(),
    s_url       :: undefined | binary(),

    %% SendBinarySms extention
    binary_body :: undefined | binary(),
    data_coding :: undefined | binary(),
    esm_class   :: undefined | binary(),
    protocol_id :: undefined | binary()
}).

-define(authError, <<"Access denied. Check your account settings">>).
-define(originatorNotAllowedError, <<"Specified originator is not allowed">>).
-define(noAnyDestAddrError, <<"None recipient is specified or available due to your permissions">>).
-define(invalidDefDateFormatError,
        <<"defDate is invalid. defDate format is MM/DD/YYYY HH:MM">>).

-record('DOWN',{
    ref             :: reference(),
    type = process      :: process,
    object              :: pid(),
    info            :: term() | noproc | noconnection
}).

-endif. % soap_srv.hrl
