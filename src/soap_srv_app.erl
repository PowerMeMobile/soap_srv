-module(soap_srv_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% API
-export([
    get_env/0
]).

-include_lib("alley_common/include/application_spec.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
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
% see soap_srv start script
-spec get_env() -> production | develop.
get_env() ->
    case init:get_argument(environment) of
        {ok, [["develop"]]} -> develop;
        _ -> production
    end.
