-module(soap_srv_auth).

%% TODO
%% Remove from cache RPC call to make cache consistent
%% with Kelly

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

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
    chan                    :: pid(),
    chan_mon_ref            :: reference(),
    reply_to                :: binary(),
    pending_workers = []    :: [#pworker{}],
    pending_responses = []  :: [#presponse{}]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec authenticate(binary(), binary(), binary()) ->
    {ok, Customer :: #k1api_auth_response_dto{}} |
    {error, timeout}.
authenticate(CustomerID, UserID, Password) ->
    User = {CustomerID, UserID, Password},
    authenticate(check_cache, User).

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
    case setup_chan(#st{}) of
        {ok, St} ->
            lager:info("auth_srv: started"),
            {ok, St};
        unavailable ->
            lager:error("auth_srv: initializing failed (amqp_unavailable). shutdown"),
            {stop, amqp_unavailable}
    end.

handle_call(get_channel, _From, St = #st{}) ->
    {reply, {ok, St#st.chan}, St};

handle_call({get_response, MesID}, From, St = #st{}) ->
    WList = St#st.pending_workers,
    RList = St#st.pending_responses,
    Worker = #pworker{
        id = MesID,
        from = From,
        timestamp = soap_srv_utils:get_now()
    },
    {ok, NRList, NWList} =
        soap_srv_utils:process_worker_request(Worker, RList, WList),
    {noreply, St#st{
        pending_workers = NWList,
        pending_responses = NRList
    }};

handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(_Msg, St) ->
    {stop, unexpected_cast, St}.

handle_info(#'DOWN'{ref = Ref, info = Info}, St = #st{chan_mon_ref = Ref}) ->
    lager:error("auth_srv: amqp channel down (~p)", [Info]),
    {stop, amqp_channel_down, St};

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
                timestamp = soap_srv_utils:get_now(),
                response = AuthResp
            },
            {ok, NRList, NWList} =
                soap_srv_utils:process_response(Response, ResponsesList, WorkersList),
            {noreply, St#st{
                pending_workers = NWList,
                pending_responses = NRList
            }};
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

setup_chan(St = #st{}) ->
    case rmql:channel_open() of
        {ok, Channel} ->
            MonRef = erlang:monitor(process, Channel),
            ok = rmql:queue_declare(Channel, ?AuthResponseQueue, []),
            ok = rmql:queue_declare(Channel, ?AuthRequestQueue, []),
            NoAck = true,
            {ok, _ConsumerTag} = rmql:basic_consume(Channel, ?AuthResponseQueue, NoAck),
            {ok, St#st{chan = Channel, chan_mon_ref = MonRef}};
        unavailable -> unavailable
    end.

authenticate(check_cache, User) ->
    {CustomerID, UserID, Password} = User,
    case soap_srv_auth_cache:fetch(CustomerID, UserID, Password) of
        {ok, Customer} ->
            {ok, Customer};
        not_found ->
            authenticate(request_backend, User)
    end;

authenticate(request_backend, User) ->
    {CustomerID, UserID, Password} = User,
    {ok, RequestID} = request_backend_auth(CustomerID, UserID, Password),
    try get_auth_response(RequestID) of
        {ok, Customer} ->
            ok = soap_srv_auth_cache:store(CustomerID, UserID, Password, Customer),
            {ok, Customer}
    catch
        _:{timeout, _} -> {error, timeout}
    end.

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
