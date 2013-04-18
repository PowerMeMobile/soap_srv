-module(soap_ehttp_handler).

-behaviour(cowboy_http_handler).

-export([
	init/3,
	handle/2,
	terminate/3
]).

init({tcp, http}, Req, _Opt) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	Resp = <<"Not found: mistake in the host or path of the service URI">>,
	{ok, Req2} = cowboy_req:reply(404, [], Resp, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
