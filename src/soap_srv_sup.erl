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
        ?CHILD(soap_srv_pdu_logger_sup, infinity, supervisor),
        ?CHILD(soap_srv_http_in_logger, 5000, worker),
        ?CHILD(soap_srv_http_out_logger, 5000, worker),
        ?CHILD(alley_services_auth_cache, 5000, worker),
        ?CHILD(alley_services_auth, 5000, worker),
        ?CHILD(soap_srv_defer, 5000, worker),
        ?CHILD(alley_services_api, 5000, worker),
        ?CHILD(alley_services_blacklist, 5000, worker),
        ?CHILD(soap_srv_mt, 5000, worker),
        ?CHILD(soap_srv_mo, 5000, worker)
    ]}}.
