-module(soap_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Timeout, Type), {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).

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
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(soap_srv_defer, 5000, worker),
        ?CHILD(soap_srv_mt, 5000, worker),
        ?CHILD(soap_srv_mo, 5000, worker)
    ]}}.
