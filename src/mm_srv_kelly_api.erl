-module(mm_srv_kelly_api).

-ignore_xref([{start_link, 0}]).

-export([
    start_link/0
]).

%% API
-export([
    get_coverage/3,
    get_blacklist/3,
    get_delivery_status/4,
    retrieve_sms/4,

    subscribe_incoming_sms/8,
    unsubscribe_incoming_sms/4,
    subscribe_sms_receipts/6,
    unsubscribe_sms_receipts/4
]).

-include("logging.hrl").
-include("application.hrl").
-include_lib("alley_dto/include/adto.hrl").

-type customer_id() :: binary().
-type user_id()     :: binary().
-type version()     :: binary().
-type request_id()  :: binary().
-type src_addr()    :: binary().
-type dst_addr()    :: binary().
-type subscription_id() :: binary().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, QueueName} = application:get_env(?APP, kelly_api_queue),
    rmql_rpc_client:start_link(?MODULE, QueueName).

-spec get_coverage(customer_id(), user_id(), version()) ->
    {ok, [#k1api_coverage_response_dto{}]} | {error, timeout}.
get_coverage(CustomerId, UserId, Version) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #k1api_coverage_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        version = Version
    },
    ?log_debug("Sending coverage request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"CoverageReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_coverage_response_dto{}, RespBin) of
                {ok, Resp = #k1api_coverage_response_dto{}} ->
                    ?log_debug("Got coverage response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Coverage response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_debug("Got coverage response: timeout", []),
            {error, timeout}
    end.

-spec get_blacklist(customer_id(), user_id(), version()) ->
    {ok, [#k1api_blacklist_response_dto{}]} | {error, timeout}.
get_blacklist(CustomerId, UserId, Version) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #k1api_blacklist_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        version = Version
    },
    ?log_debug("Sending blacklist request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"BlacklistReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_blacklist_response_dto{}, RespBin) of
                {ok, Resp = #k1api_blacklist_response_dto{}} ->
                    ?log_debug("Got blacklist response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Coverage blacklist decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_debug("Got blacklist response: timeout", []),
            {error, timeout}
    end.

-spec get_delivery_status(customer_id(), user_id(), request_id(), src_addr()) ->
    {ok, [#k1api_sms_delivery_status_response_dto{}]} | {error, timeout}.
get_delivery_status(CustomerId, UserId, SmsReqId, SenderAddr) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #k1api_sms_delivery_status_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        sms_request_id = SmsReqId,
        address = SenderAddr
    },
    ?log_debug("Sending delivery status request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"DeliveryStatusReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_sms_delivery_status_response_dto{}, RespBin) of
                {ok, Resp = #k1api_sms_delivery_status_response_dto{}} ->
                    ?log_debug("Got delivery status response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Delivery status response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_debug("Got delivery status response: timeout", []),
            {error, timeout}
    end.

-spec retrieve_sms(customer_id(), user_id(), dst_addr(), pos_integer()) ->
    {ok, [#k1api_retrieve_sms_response_dto{}]} | {error, timeout}.
retrieve_sms(CustomerId, UserId, DestAddr, BatchSize) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    Req = #k1api_retrieve_sms_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        dest_addr = DestAddr,
        batch_size = BatchSize
    },
    ?log_debug("Sending retrieve sms request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"RetrieveSmsReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_retrieve_sms_response_dto{}, RespBin) of
                {ok, Resp = #k1api_retrieve_sms_response_dto{}} ->
                    ?log_debug("Got retrieve sms response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Retrive sms response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_error("Got retrive sms response: timeout", []),
            {error, timeout}
    end.

-spec subscribe_incoming_sms(
    request_id(), customer_id(), user_id(), dst_addr(),
    binary(), binary(), binary(), binary()
) ->
    {ok, #k1api_subscribe_incoming_sms_response_dto{}} | {error, term()}.
subscribe_incoming_sms(
    ReqId, CustomerId, UserId, DestAddr,
    NotifyUrl, Criteria, Correlator, CallbackData
) ->
    Req = #k1api_subscribe_incoming_sms_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        dest_addr = DestAddr,
        notify_url = NotifyUrl,
        criteria = Criteria,
        correlator = Correlator,
        callback_data = CallbackData
    },
    ?log_debug("Sending subscribe incoming sms request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"SubscribeIncomingSmsReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_subscribe_incoming_sms_response_dto{}, RespBin) of
                {ok, Resp} ->
                    ?log_debug("Got subscribe incoming sms response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Subscribe incoming sms response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_error("Subscribe incoming sms response: timeout", []),
            {error, timeout}
    end.

-spec unsubscribe_incoming_sms(
    request_id(), customer_id(), user_id(), subscription_id()
) ->
    {ok, #k1api_unsubscribe_incoming_sms_response_dto{}} | {error, term()}.
unsubscribe_incoming_sms(ReqId, CustomerId, UserId, SubscriptionId) ->
    Req = #k1api_unsubscribe_incoming_sms_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        subscription_id = SubscriptionId
    },
    ?log_debug("Sending unsubscribe incoming sms request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"UnsubscribeIncomingSmsReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_unsubscribe_incoming_sms_response_dto{}, RespBin) of
                {ok, Resp} ->
                    ?log_debug("Got unsubscribe incoming sms response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Unsubscribe incoming sms response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_error("Unsubscribe incoming sms response: timeout", []),
            {error, timeout}
    end.

-spec subscribe_sms_receipts(
    request_id(), customer_id(), user_id(),
    binary(), dst_addr(), binary()
) ->
    {#k1api_subscribe_sms_receipts_response_dto{}} | {error, term()}.
subscribe_sms_receipts(
    ReqId, CustomerId, UserId,
    NotifyUrl, DestAddr, CallbackData
) ->
    Req = #k1api_subscribe_sms_receipts_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        url = NotifyUrl,
        dest_addr = DestAddr,
        callback_data = CallbackData
    },
    ?log_debug("Sending subscribe sms receipts request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"SubscribeSmsReceiptsReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_subscribe_sms_receipts_response_dto{}, RespBin) of
                {ok, Resp} ->
                    ?log_debug("Got subscribe sms receipts sms response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Subscribe sms receipts response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_error("Subscribe sms receipts response: timeout", []),
            {error, timeout}
    end.

-spec unsubscribe_sms_receipts(
    request_id(), customer_id(), user_id(), subscription_id()
) ->
    {ok, #k1api_unsubscribe_sms_receipts_response_dto{}} | {error, term()}.
unsubscribe_sms_receipts(ReqId, CustomerId, UserId, SubscriptionId) ->
    Req = #k1api_unsubscribe_sms_receipts_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        subscription_id = SubscriptionId
    },
    ?log_debug("Sending unsubscribe sms receipts request: ~p", [Req]),
    {ok, ReqBin} = adto:encode(Req),
    case rmql_rpc_client:call(?MODULE, ReqBin, <<"UnsubscribeSmsReceiptsReq">>) of
        {ok, RespBin} ->
            case adto:decode(#k1api_unsubscribe_sms_receipts_response_dto{}, RespBin) of
                {ok, Resp} ->
                    ?log_debug("Got unsubscribe sms receipts sms response: ~p", [Resp]),
                    {ok, Resp};
                {error, Error} ->
                    ?log_error("Unsubscribe sms receipts response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_error("Unsubscribe sms receipts response: timeout", []),
            {error, timeout}
    end.
