-module(soap_srv_app).

%% TODO
%% Move all start functions to sup with gen_server to make
%% start order obvious

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([set_debug_level/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	soap_db:init_mnesia(),
    {ok, SupervisorPid} = soap_srv_sup:start_link(),
	soap_srv_protocol:init(),
	{ok, SupervisorPid}.

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

set_debug_level() ->
	lager:set_loglevel(lager_console_backend, debug).
