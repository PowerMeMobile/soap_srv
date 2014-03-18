-module(soap_srv_test_echo_handler).

-behaviour(cowboy_http_handler).

%% API
-export([
    init/0
]).

%% Cowboy callbacks
-export([
    init/3,
    handle/2,
    terminate/3
]).

%% ===================================================================
%% API
%% ===================================================================

-spec init() -> ok.
init() ->
    case soap_srv_app:get_env() of
        develop ->
            ProtocolOpts = [
                {env, [{dispatch, dispatch_rules()}]}
            ],
            Port  = 4444,
            {ok, _Pid} =
            cowboy:start_http(?MODULE, 1, [{port, Port}], ProtocolOpts),
            lager:info("test_echo_handler: started [0.0.0.0:~p]", [Port]);
        _ ->
            lager:info("test_echo_handler: not started", [])
    end.

%% ===================================================================
%% Internals
%% ===================================================================

dispatch_rules() ->
    DispatchRaw =
        [{'_', [
            {'_', ?MODULE, []}]
        }],
    cowboy_router:compile(DispatchRaw).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

-spec init({tcp, http}, cowboy_req:req(), []) ->
    {ok, cowboy_req:req(), undefined}.
init({tcp, http}, Req, []) ->
    {ok, Req, undefined}.

-spec handle(cowboy_req:req(), undefined) ->
    {ok, cowboy_req:req(), undefined}.
handle(Req, State) ->
    {ok, Req2} =
        cowboy_req:reply(200, [], <<"OK">>, Req),
    {ok, Req2, State}.

-spec terminate(term(), cowboy_req:req(), undefined) -> ok.
terminate(_Reason, _Req, _St) ->
    ok.
