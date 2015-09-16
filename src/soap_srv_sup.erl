-module(soap_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

-define(CHILD(I, Restart, Timeout, Type), {I, {I, start_link, []}, Restart, Timeout, Type, [I]}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    %% We sustain 100 logger crashes per sec.
    %% If it's not enough set logger level to none.
    {ok, {{one_for_one, 100, 1}, [
        ?CHILD(alley_services_http_in_logger, permanent, 5000, worker)
    ]}}.
