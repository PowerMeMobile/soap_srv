-module(soap_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [

		{soap_srv_plogger_sup,
			{soap_srv_plogger_sup, start_link, []},
			permanent, infinity, supervisor, [soap_srv_plogger_sup]},

		?CHILD(soap_srv_http_logger, worker),

		?CHILD(soap_auth_cache, worker),

		?CHILD(soap_auth_srv, worker),

		?CHILD(soap_mt_srv, worker)

	]} }.

