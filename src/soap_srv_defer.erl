-module(soap_srv_defer).

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

%% API
-export([
    start_link/0,
    defer/3
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

-include("logging.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(st, {
}).

-define(checkInterval, (1000 * 60 * 1)).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

defer(ReqID, TimeStamp, Req) ->
    ok = gen_server:call(?MODULE, {defer, ReqID, TimeStamp, Req}).

%% ===================================================================
%% GenServer Callback Functions Definitions
%% ===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, ?MODULE} = dets:open_file(?MODULE, [{file, "data/defered_requests.dets"}]),
    ?log_info("def_srv: started", []),
    {ok, #st{}, ?checkInterval}.

handle_call({defer, ReqID, TimeStamp, Req}, _From, St) ->
    ok = dets:insert(?MODULE, {ReqID, TimeStamp, Req}),
    {reply, ok, St, ?checkInterval};
handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(Req, St) ->
    {stop, {unexpected_cast, Req}, St}.

handle_info(timeout, St) ->
    TS = os:timestamp(),
    Defered = qlc:e(qlc:q(
        [R || R <- dets:table(?MODULE), element(2, R) < TS]
    )),
    [send(Task) || Task <- Defered],
    {noreply, St, ?checkInterval};
handle_info(_Info, St) ->
    {stop, unexpected_info, St}.

terminate(Reason, _St) ->
    dets:close(?MODULE),
    ?log_info("def_srv: terminated (~p)", [Reason]).

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Internals
%% ===================================================================

send({ID,_, Req}) ->
    ok = soap_srv_mt:publish(Req),
    ok = dets:delete(?MODULE, ID).
