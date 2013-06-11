-module(soap_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([
	get_env/0
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	set_lager_loglevel(),
	soap_srv_db:init_mnesia(),
    {ok, SupervisorPid} = soap_srv_sup:start_link(),
	soap_srv_protocol:init(),
	soap_srv_test_echo_handler:init(),
	{ok, SupervisorPid}.

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

% uses erl -environment develop for dev mode
% see soap_srv start scrip
-spec get_env() -> production | develop.
get_env() ->
	case init:get_argument(environment) of
		{ok, [["develop"]]} -> develop;
		_ -> production
	end.

%% ===================================================================
%% Internals
%% ===================================================================

set_lager_loglevel() ->
	case get_env() of
		develop -> lager:set_loglevel(lager_console_backend, debug);
		_ -> ok
	end.
