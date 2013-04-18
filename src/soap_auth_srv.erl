-module(soap_auth_srv).

%% TODO
%% Check specs

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	authenticate/3
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

-define(AuthRequestQueue, <<"pmm.k1api.auth_request">>).
-define(AuthResponseQueue, <<"pmm.k1api.auth_response">>).

-record(st, {
	chan 					:: pid(),
	reply_to 				:: binary(),
	pending_workers = [] 	:: [#pworker{}],
	pending_responses = [] 	:: [#presponse{}]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec authenticate(binary(), binary(), binary()) ->
	{ok, Customer :: any()}.
authenticate(CustomerID, UserID, Pswd) ->
	case soap_auth_cache:fetch(CustomerID, UserID, Pswd) of
		{ok, Customer} ->
			lager:debug("User found in cache", []),
			{ok, Customer};
		not_found ->
			lager:debug("User NOT found in cache", []),
			{ok, RequestID} = request_backend_auth(CustomerID, UserID, Pswd),
			lager:debug("Sent auth request [id: ~p]", [RequestID]),
			{ok, Customer} = get_auth_response(RequestID),
			ok = soap_auth_cache:store(CustomerID, UserID, Pswd, Customer),
			lager:debug("Got sucessful auth response", []),
			{ok, Customer}
	end.

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Chan} = rmql:channel_open(Connection),
	link(Chan),
	ok = rmql:queue_declare(Chan, ?AuthResponseQueue, []),
	ok = rmql:queue_declare(Chan, ?AuthRequestQueue, []),
	NoAck = true,
	{ok, _ConsumerTag} = rmql:basic_consume(Chan, ?AuthResponseQueue, NoAck),
	{ok, #st{chan = Chan}}.

handle_call(get_channel, _From, St = #st{}) ->
	{reply, {ok, St#st.chan}, St};

handle_call({get_response, MesID}, From, St = #st{}) ->
	WList = St#st.pending_workers,
	RList = St#st.pending_responses,
	Worker = #pworker{
				id = MesID,
				from = From,
				timestamp = soap_utils:get_now()
			},
	{ok, NRList, NWList} =
		soap_utils:process_worker_request(Worker, RList, WList),
	{noreply, St#st{pending_workers = NWList, pending_responses = NRList}};

handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(_Msg, St) ->
    {stop, unexpected_cast, St}.

handle_info({#'basic.deliver'{}, AmqpMsg = #amqp_msg{}}, St = #st{}) ->
	Content = AmqpMsg#amqp_msg.payload,
	ResponsesList = St#st.pending_responses,
	WorkersList = St#st.pending_workers,
	lager:debug("Got auth response", []),
	case adto:decode(#k1api_auth_response_dto{}, Content) of
		{ok, AuthResp = #k1api_auth_response_dto{}} ->
			CorrelationID = AuthResp#k1api_auth_response_dto.id,
			lager:debug("AuthResponse was sucessfully decoded [id: ~p]", [CorrelationID]),
			Response = #presponse{
							id = CorrelationID,
							timestamp = soap_utils:get_now(),
							response = AuthResp
						},
			{ok, NRList, NWList} =
				soap_utils:process_response(Response, ResponsesList, WorkersList),
			{noreply, St#st{
							pending_workers = NWList,
							pending_responses = NRList}};
		{error, Error} ->
			lager:error("Failed To Decode Auth Response Due To ~p : ~p", [Error, Content]),
			{noreply, St}
	end;

handle_info(_Info, St) ->
    {stop, unexpected_info, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Internal
%% ===================================================================

get_channel() ->
	gen_server:call(?MODULE, get_channel).

get_auth_response(RequestUUID) ->
	gen_server:call(?MODULE, {get_response, RequestUUID}).

request_backend_auth(CustomerID, UserID, Password) ->
 	{ok, Channel} = get_channel(),
	RequestUUID = uuid:unparse(uuid:generate_time()),
    AuthRequest = #k1api_auth_request_dto{
        id = RequestUUID,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password
    },
	{ok, Payload} = adto:encode(AuthRequest),
    Props = #'P_basic'{},
    ok = rmql:basic_publish(Channel, ?AuthRequestQueue, Payload, Props),
	{ok, RequestUUID}.
