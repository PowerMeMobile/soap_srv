-module(soap_srv_pdu_logger_sup).

-behaviour(supervisor).

-ignore_xref([{start_link, 0}]).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        {soap_srv_pdu_logger_sup, {soap_srv_pdu_logger, start_link, []},
            temporary, 5000, worker, [soap_srv_pdu_logger]}
    ]}}.
