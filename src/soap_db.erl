-module(soap_db).

%% TODO
%% next_id(CustomerID, UserID).

%% API
-export([
	init_mnesia/0,
	next_id/1, next_id/2
]).

-type customer_id() 	:: binary().

-record(customer_next_message_id, {
	customer_id 		:: customer_id(),
	next_id 			:: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec next_id(customer_id()) -> {ok, [integer()]}.
next_id(CustomerID) ->
	next_id(CustomerID, 1).

-spec next_id(customer_id(), NumberOfIDs :: integer()) -> {ok, [integer()]}.
next_id(CustomerID, NumberOfIDs) ->
	{atomic, IDs} = mnesia:transaction(fun() ->
		case mnesia:read(customer_next_message_id, CustomerID, write) of
			[] ->
				update_counter(1, NumberOfIDs, CustomerID);
			[#customer_next_message_id{next_id = NextID}] ->
				update_counter(NextID, NumberOfIDs, CustomerID)
		end
	end),
	{ok, IDs}.

-spec init_mnesia() -> ok.
init_mnesia() ->
	Nodes = [node()],
	%% mnesia:set_debug_level(verbose),
	mnesia:stop(),
	lager:info("db: creating mnesia schema on: ~p...", [Nodes]),
	ok = case mnesia:create_schema(Nodes) of
		ok ->
			lager:info("db: schema was created", []),
			ok;
		{error, {MnesiaNode, {already_exists, MnesiaNode}}} ->
			MnesiaNodes = mnesia:system_info(db_nodes),
			case lists:member(MnesiaNode, MnesiaNodes) of
				true ->
					lager:info("db: mnesia schema already exists on: ~p", [MnesiaNode]),
					ok;
				false ->
					lager:error("Mnesia schema already exists on: ~p, but it's not in existing list: ~p"
						" Did you rename the node?",
						[MnesiaNode, MnesiaNodes]),
					erlang:error(schema_already_exists_created_on_different_node)
			end
	end,
	ok = mnesia:start(),

	ok = ensure_table(
		customer_next_message_id,
		record_info(fields, customer_next_message_id)).

%% ===================================================================
%% Local Functions
%% ===================================================================

update_counter(NextID, NumberOfIDs, CustomerID) ->
	Max = 999999999,
	{From, To, NewNextID} =
	case (NextID + NumberOfIDs - 1) > Max of
		true -> {1, NumberOfIDs, NumberOfIDs + 1};
		false -> {NextID, NextID + NumberOfIDs - 1, NextID + NumberOfIDs}
	end,
	IDs = lists:seq(From, To),
	mnesia:write(#customer_next_message_id{customer_id = CustomerID, next_id = NewNextID}),
	IDs.

ensure_table(TableName, RecordInfo) ->
	ok = case mnesia:create_table(TableName, [
					{disc_copies, [node()]},
					{attributes, RecordInfo}]) of
			{atomic, ok} ->
				ok;
			{aborted, {already_exists, TableName}} ->
				ok
		 end,
    ok = mnesia:wait_for_tables([TableName], infinity).
