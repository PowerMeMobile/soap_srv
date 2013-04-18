-module(soap_utils).

-export([
	addr_to_dto/1,
	get_now/0,
	process_response/3,
	process_worker_request/3
]).

-include_lib("alley_dto/include/adto.hrl").
-include("soap_srv.hrl").

%% ===================================================================
%% Addr to dto
%% ===================================================================

-spec addr_to_dto(Addr :: binary()) -> #addr{}.
addr_to_dto(AddrBin) when is_binary(AddrBin) ->
	Addr = binary_to_list(AddrBin),
	IsInteger =
	try	list_to_integer(Addr) of
		_ -> true
	catch
		_:_ -> false
	end,
	Length = length(Addr),
	addr_to_dto(AddrBin, IsInteger, Length).

addr_to_dto(AddrBin, true, Length) when Length < 7 -> % 1..6
	#addr{
		addr = AddrBin,
		ton = 6,
		npi = 0
	};
addr_to_dto(AddrBin, true, _Length) -> % 7..
	#addr{
		addr = AddrBin,
		ton = 1,
		npi = 1
	};
addr_to_dto(AddrBin, false, _Length) ->
	#addr{
		addr = AddrBin,
		ton = 5,
		npi = 0
	}.

%% ===================================================================
%% Request/Response processing
%% ===================================================================

-spec process_response(#presponse{}, [#presponse{}], [#pworker{}]) ->
	{ok, [#presponse{}], [#pworker{}]}.
process_response(PResponse = #presponse{id = ID, response = Response}, RList, WList) ->
		case lists:keytake(ID, #pworker.id, WList) of
		{value, #pworker{from = From}, RestWorkerList} ->
			gen_server:reply(From, {ok, Response}),
			{ok, purge(RList), purge(RestWorkerList)};
		false ->
			{ok, [PResponse] ++ purge(RList), purge(WList)}
	end.

-spec process_worker_request(#pworker{}, [#presponse{}], [#pworker{}]) ->
	{ok, [#presponse{}], [#pworker{}]}.
process_worker_request(Worker = #pworker{id = ItemID, from = From}, RList, WList) ->
	case lists:keytake(ItemID, #presponse.id, RList) of
		{value, #presponse{response = Response}, RestRespList} ->
			gen_server:reply(From, {ok, Response}),
			{ok, purge(RestRespList), purge(WList)};
		false ->
			{ok, purge(RList), [Worker] ++ purge(WList)}
	end.

-spec get_now() -> integer().
get_now() ->
	 calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%% ===================================================================
%% Internals
%% ===================================================================

purge(List) ->
	purge(List, [], get_now() - 5).

purge([], Acc, _Now) -> Acc;
purge([#pworker{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([#presponse{timestamp = TS} | RestList], Acc, Now) when Now >= TS ->
	purge(RestList, Acc, Now);
purge([Item | RestList], Acc, Now) ->
	purge(RestList, [Item | Acc], Now).
