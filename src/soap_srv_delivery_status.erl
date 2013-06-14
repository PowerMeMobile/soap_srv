-module(soap_srv_delivery_status).

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

%% API
-export([
	start_link/0,
	get/3
]).

%% GenServer Callbacks
-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include("soap_srv.hrl").

-define(deliveryStatusRequestQueue, <<"pmm.k1api.delivery_status_request">>).
-define(deliveryStatusResponseQueue, <<"pmm.k1api.delivery_status_response">>).

-record(state, {
	chan 					:: pid(),
	ref 					:: reference(),
	reply_to 				:: binary(),
	pending_workers = []	:: [#pworker{}],
	pending_responses = [] 	:: [#presponse{}]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(binary(), binary(), binary()) -> {ok, [#k1api_sms_status_dto{}]}.
get(CustomerUUID, UserID, SendSmsRequestId) ->
	{ok, RequestID} = request_backend(CustomerUUID, UserID, SendSmsRequestId),
	lager:debug("Successfully sent request [~p] to backend", [RequestID]),
	{ok, Response} = get_response(RequestID),
	{ok, Response#k1api_sms_delivery_status_response_dto.statuses}.

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
	{ok, Chan} = rmql:channel_open(),
	Ref = erlang:monitor(process, Chan),
	ok = rmql:queue_declare(Chan, ?deliveryStatusResponseQueue, []),
	ok = rmql:queue_declare(Chan, ?deliveryStatusRequestQueue, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?deliveryStatusResponseQueue, NoAck),
	{ok, #state{chan = Chan, ref = Ref}}.

handle_call(get_channel, _From, State = #state{chan = Chan}) ->
	{reply, {ok, Chan}, State};

handle_call({get_response, MesID}, From,
					State = #state{
								pending_workers = WList,
								pending_responses = RList}) ->
	Worker = #pworker{id = MesID, from = From, timestamp = soap_srv_utils:get_now()},
	{ok, NRList, NWList} = soap_srv_utils:process_worker_request(Worker, RList, WList),
	{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

handle_info(#'DOWN'{ref = Ref, info = Info}, St = #state{ref = Ref}) ->
	lager:error("mt_srv: amqp channel down (~p)", [Info]),
	{stop, amqp_channel_down, St};

handle_info({#'basic.deliver'{}, #amqp_msg{payload = Content}},
			 State = #state{
			 	pending_responses = ResponsesList,
				pending_workers = WorkersList}) ->
	lager:debug("Got sms delivery status response", []),
	case adto:decode(#k1api_sms_delivery_status_response_dto{}, Content) of
		{ok, Response = #k1api_sms_delivery_status_response_dto{
				id = CorrelationID }} ->
			lager:debug("Response was sucessfully decoded [id: ~p]", [CorrelationID]),
			NewPendingResponse = #presponse{id = CorrelationID, timestamp = soap_srv_utils:get_now(), response = Response},
			{ok, NRList, NWList} = soap_srv_utils:process_response(NewPendingResponse, ResponsesList, WorkersList),
			{noreply, State#state{pending_workers = NWList, pending_responses = NRList}};
		{error, Error} ->
			lager:error("Failed To Decode Response Due To ~p : ~p", [Error, Content]),
			{noreply, State}
	end;

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

get_channel() ->
	gen_server:call(?MODULE, get_channel).

get_response(RequestUUID) ->
	gen_server:call(?MODULE, {get_response, RequestUUID}).

request_backend(CustomerUUID, UserID, SendSmsRequestId) ->
 	{ok, Channel} = get_channel(),
	RequestUUID = uuid:unparse(uuid:generate_time()),
	DeliveryStatusReqDTO = #k1api_sms_delivery_status_request_dto{
		id = RequestUUID,
		customer_id = CustomerUUID,
		user_id = UserID,
		sms_request_id = SendSmsRequestId,
		address = soap_srv_utils:addr_to_dto(<<>>)
	},
	lager:debug("DeliveryStatusReqDTO: ~p", [DeliveryStatusReqDTO]),
	{ok, Payload} = adto:encode(DeliveryStatusReqDTO),
    ok = rmql:basic_publish(Channel, ?deliveryStatusRequestQueue, Payload, #'P_basic'{}),
	{ok, RequestUUID}.
