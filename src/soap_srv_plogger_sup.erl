-module(soap_srv_plogger_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {{simple_one_for_one, 0, 1},
			[{soap_srv_plogger_sup,
				{soap_srv_pdu_logger, start_link, []},
				temporary, 5000, worker, [soap_srv_pdu_logger]}]}}.
