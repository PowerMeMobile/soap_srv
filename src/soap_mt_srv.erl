-module(soap_mt_srv).

%% TODO
%% AllowedAddresses
%% SwitchOff receipts on Kelly side
%% refactor get_ids fun to avoid double iteration

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	process/1
]).

%% GenServer Callbacks
-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	code_change/3,
	terminate/2
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include("soap_srv.hrl").

-define(SmsRequestQueue, <<"pmm.k1api.sms_request">>).

-define(just_sms_request_param(Name, Param),
	apply(fun
		(undefined) ->
			[];
		(Str) when is_binary(Str) ; is_list(Str) ->
			{just_sms_request_param_dto, Name, {string, Str}};
		(Bool) when Bool =:= true ; Bool =:= false ->
			{just_sms_request_param_dto, Name, {boolean, Bool}};
		(Int) when is_integer(Int) ->
			{just_sms_request_param_dto, Name, {integer, Int}}
	end, [Param])).

-record(unconfirmed, {
	id		:: integer(),
	from	:: term()
}).

-record(st, {
	chan 		:: pid(),
	next_id = 1 :: integer()
}).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec process(#send_sms_req{}) ->
	{ok, RequestID :: binary()} |
	{error, Reason :: any()}.
process(SendSmsReq = #send_sms_req{}) ->
	{ok, Customer} =
		soap_auth_srv:authenticate(
			SendSmsReq#send_sms_req.customer_id,
			SendSmsReq#send_sms_req.user_name,
			SendSmsReq#send_sms_req.password),

	{Encoding, Encoded} =
		case gsm0338:from_utf8(SendSmsReq#send_sms_req.text) of
			{valid, Binary} -> {default, Binary};
			{invalid, Binary} -> {ucs2, Binary}
		end,
	NumberOfSymbols = size(Encoded),

	{ok, NumberOfParts} = get_message_parts(NumberOfSymbols, Encoding),
	lager:debug("Encoded message: ~p, Encoding: ~p, Symbols: ~p, Parts: ~p",
		[Encoded, Encoding, NumberOfSymbols, NumberOfParts]),
	send(SendSmsReq, Customer, Encoding, NumberOfParts);
process(SendServiceSmsReq = #send_service_sms_req{}) ->
	{ok, Customer} =
		soap_auth_srv:authenticate(
			SendServiceSmsReq#send_service_sms_req.customer_id,
			SendServiceSmsReq#send_service_sms_req.user_name,
			SendServiceSmsReq#send_service_sms_req.password),
	Message =
		<<"<%SERVICEMESSAGE:",
		(SendServiceSmsReq#send_service_sms_req.service_name)/binary,	";",
		(SendServiceSmsReq#send_service_sms_req.service_url)/binary, "%>">>,
	{Encoding, Encoded} =
		case gsm0338:from_utf8(Message) of
			{valid, Binary} -> {default, Binary};
			{invalid, Binary} -> {ucs2, Binary}
		end,
	NumberOfSymbols = size(Encoded),

	{ok, NumberOfParts} = get_message_parts(NumberOfSymbols, Encoding),
	lager:debug("Encoded message: ~p, Encoding: ~p, Symbols: ~p, Parts: ~p",
		[Encoded, Encoding, NumberOfSymbols, NumberOfParts]),
		#k1api_auth_response_dto{
		uuid = CustomerID,
		allowed_sources = _AllowedSources,
		default_validity = DefaultValidity,
		no_retry = NoRetry
	} = Customer,
	ReqID = uuid:unparse(uuid:generate_time()),
	Params = lists:flatten([
			?just_sms_request_param(<<"registered_delivery">>, false),
			?just_sms_request_param(<<"service_type">>, <<>>),
			?just_sms_request_param(<<"no_retry">>, NoRetry),
			?just_sms_request_param(<<"validity_period">>, fmt_validity(DefaultValidity)),
			?just_sms_request_param(<<"priority_flag">>, 0),
			?just_sms_request_param(<<"esm_class">>, 64),
			?just_sms_request_param(<<"protocol_id">>, 0),
			?just_sms_request_param(<<"data_coding">>, 245),
			?just_sms_request_param(<<"source_port">>, 9200),
			?just_sms_request_param(<<"destination_port">>, 2948)
			]),
	AllDestinations = SendServiceSmsReq#send_service_sms_req.recipients,
	{Destinations, Rejected} =
	get_allowed_destinations(AllDestinations, Customer#k1api_auth_response_dto.networks),
	NumberOfDests = length(Destinations),
	GtwID = get_suitable_gtw(Customer, NumberOfDests),
	MessageIDs = get_ids(CustomerID, NumberOfDests, NumberOfParts),
   	lager:debug("Message IDs: ~p", [MessageIDs]),
	DTO = #just_sms_request_dto{
		id = ReqID,
		gateway_id = GtwID,
		customer_id = CustomerID,
		user_id = SendServiceSmsReq#send_service_sms_req.user_name,
		client_type = k1api,
		type = regular,
		message = Message,
		encoding = Encoding,
		params = Params,
		source_addr = SendServiceSmsReq#send_service_sms_req.originator,
		dest_addrs = {regular, Destinations},
		message_ids = MessageIDs
	},
	lager:debug("Built SmsRequest: ~p", [DTO]),
	{ok, Bin} = adto:encode(DTO),
	soap_srv_pdu_logger:log(DTO),
	lager:debug("SmsRequest was sucessfully encoded", []),
	ok = publish_sms_request(Bin, ReqID, GtwID),
	{ok, ReqID, Rejected};

process(SendBinarySmsReq = #send_binary_sms_req{}) ->

	{ok, Customer} =
		soap_auth_srv:authenticate(
			SendBinarySmsReq#send_binary_sms_req.customer_id,
			SendBinarySmsReq#send_binary_sms_req.user_name,
			SendBinarySmsReq#send_binary_sms_req.password),

	#k1api_auth_response_dto{
		uuid = CustomerID,
		allowed_sources = _AllowedSources,
		default_validity = DefaultValidity,
		no_retry = NoRetry
	} = Customer,
	ReqID = uuid:unparse(uuid:generate_time()),
	Params = lists:flatten([
			?just_sms_request_param(<<"registered_delivery">>, false),
			?just_sms_request_param(<<"service_type">>, <<>>),
			?just_sms_request_param(<<"no_retry">>, NoRetry),
			?just_sms_request_param(<<"validity_period">>, fmt_validity(DefaultValidity)),
			?just_sms_request_param(<<"priority_flag">>, 0),
			?just_sms_request_param(<<"esm_class">>, SendBinarySmsReq#send_binary_sms_req.esm_class),
			?just_sms_request_param(<<"protocol_id">>, SendBinarySmsReq#send_binary_sms_req.protocol_id)
			%% ?just_sms_request_param(<<"data_coding">>, SendBinarySmsReq#send_binary_sms_req.data_coding)
			]),
	AllDestinations = SendBinarySmsReq#send_binary_sms_req.recipients,
	{Destinations, Rejected} =
	get_allowed_destinations(AllDestinations, Customer#k1api_auth_response_dto.networks),
	NumberOfDests = length(Destinations),
	GtwID = get_suitable_gtw(Customer, NumberOfDests),
	MessageIDs = get_ids(CustomerID, NumberOfDests, 1), %% NumberOfParts
   	lager:debug("Message IDs: ~p", [MessageIDs]),
	DTO = #just_sms_request_dto{
		id = ReqID,
		gateway_id = GtwID,
		customer_id = CustomerID,
		user_id = SendBinarySmsReq#send_binary_sms_req.user_name,
		client_type = k1api,
		type = regular,
		message = SendBinarySmsReq#send_binary_sms_req.binary_body,
		encoding = default, %% data coding
		params = Params,
		source_addr = SendBinarySmsReq#send_binary_sms_req.originator,
		dest_addrs = {regular, Destinations},
		message_ids = MessageIDs
	},
	lager:debug("Built SmsRequest: ~p", [DTO]),
	{ok, Bin} = adto:encode(DTO),
	soap_srv_pdu_logger:log(DTO),
	lager:debug("SmsRequest was sucessfully encoded", []),
	ok = publish_sms_request(Bin, ReqID, GtwID),
	{ok, ReqID, Rejected}.


%% ===================================================================
%% GenServer Callback Functions Definitions
%% ===================================================================

init([]) ->
	{ok, Connection} = rmql:connection_start(),
	{ok, Channel} = rmql:channel_open(Connection),
	link(Channel),
	amqp_channel:register_confirm_handler(Channel, self()),
	ok = confirm_select(Channel),
	ok = rmql:queue_declare(Channel, ?SmsRequestQueue, []),
	?MODULE = ets:new(?MODULE, [named_table, ordered_set, {keypos, 2}]),
	{ok, #st{chan = Channel}}.

handle_call({publish, Payload, ReqID, GtwID}, From, St = #st{}) ->
	GtwQueue = binary:replace(<<"pmm.just.gateway.%id%">>, <<"%id%">>, GtwID),

	%% use rabbitMQ 'CC' extention to avoid double publish confirm per 1 request
	CC = {<<"CC">>, array, [{longstr, GtwQueue}]},
    Basic = #'P_basic'{
        content_type = <<"k1apiSmsRequest">>,
        delivery_mode = 2,
        priority = 1,
        message_id = ReqID,
		headers = [CC]
    },
	Channel = St#st.chan,
    ok = rmql:basic_publish(Channel, ?SmsRequestQueue, Payload, Basic),
	true = ets:insert(?MODULE, #unconfirmed{id = St#st.next_id, from = From}),
	{noreply, St#st{next_id = St#st.next_id + 1}};

handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(Req, St) ->
    {stop, {unexpected_cast, Req}, St}.

handle_info(Confirm, St) when is_record(Confirm, 'basic.ack');
                              is_record(Confirm, 'basic.nack') ->
	handle_confirm(Confirm),
    {noreply, St};

handle_info(_Info, St) ->
    {stop, unexpected_info, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Public Confirms
%% ===================================================================

handle_confirm(#'basic.ack'{delivery_tag = DTag, multiple = false}) ->
	reply_to(DTag, ok);
handle_confirm(#'basic.ack'{delivery_tag = DTag, multiple = true}) ->
	reply_up_to(DTag, ok);
handle_confirm(#'basic.nack'{delivery_tag = DTag, multiple = false}) ->
	reply_to(DTag, {error, nack});
handle_confirm(#'basic.nack'{delivery_tag = DTag, multiple = true}) ->
	reply_up_to(DTag, {error, nack}).

reply_up_to(DTag, Reply) ->
	IDs = unconfirmed_ids_up_to(DTag),
	[reply_to(ID, Reply) || ID <- IDs].

reply_to(DTag, Reply) when is_integer(DTag) ->
	[Unconf] = ets:lookup(?MODULE, DTag),
	gen_server:reply(Unconf#unconfirmed.from, Reply),
	true = ets:delete(?MODULE, Unconf#unconfirmed.id).

unconfirmed_ids_up_to(UpToID) ->
	case ets:first(?MODULE) of
		'$end_of_table' -> [];
		FirstID ->
			unconfirmed_ids_up_to(UpToID, [], FirstID)
	end.
unconfirmed_ids_up_to(UpToID, Acc, LastID) when LastID =< UpToID ->
	case ets:next(?MODULE, LastID) of
		'$end_of_table' -> [LastID | Acc];
		NextID ->
			unconfirmed_ids_up_to(UpToID, [LastID | Acc], NextID)
	end;
unconfirmed_ids_up_to(_UUID, Acc, _LastID) ->
	Acc.

%% ===================================================================
%% Local Functions Definitions
%% ===================================================================

send(SendSmsReq, Customer, Encoding, NumberOfParts) ->
	#k1api_auth_response_dto{
		uuid = CustomerID,
		allowed_sources = _AllowedSources,
		default_validity = DefaultValidity,
		no_retry = NoRetry
	} = Customer,
	ReqID = uuid:unparse(uuid:generate_time()),
	Params = lists:flatten([
			?just_sms_request_param(<<"registered_delivery">>, false),
			?just_sms_request_param(<<"service_type">>, <<>>),
			?just_sms_request_param(<<"no_retry">>, NoRetry),
			?just_sms_request_param(<<"validity_period">>, fmt_validity(DefaultValidity)),
			?just_sms_request_param(<<"priority_flag">>, 0),
			?just_sms_request_param(<<"esm_class">>, 3),
			?just_sms_request_param(<<"protocol_id">>, 0)
			]) ++ flash(SendSmsReq#send_sms_req.flash, Encoding),

	AllDestinations = SendSmsReq#send_sms_req.recipients,
	{Destinations, Rejected} =
	get_allowed_destinations(AllDestinations, Customer#k1api_auth_response_dto.networks),
	Message = SendSmsReq#send_sms_req.text,
	NumberOfDests = length(Destinations),
	GtwID = get_suitable_gtw(Customer, NumberOfDests),
	MessageIDs = get_ids(CustomerID, NumberOfDests, NumberOfParts),
   	lager:debug("Message IDs: ~p", [MessageIDs]),
	DTO = #just_sms_request_dto{
		id = ReqID,
		gateway_id = GtwID,
		customer_id = CustomerID,
		user_id = SendSmsReq#send_sms_req.user_name,
		client_type = k1api,
		type = regular,
		message = Message,
		encoding = Encoding,
		params = Params,
		source_addr = SendSmsReq#send_sms_req.originator,
		dest_addrs = {regular, Destinations},
		message_ids = MessageIDs
	},
	lager:debug("Built SmsRequest: ~p", [DTO]),
	{ok, Bin} = adto:encode(DTO),
	soap_srv_pdu_logger:log(DTO),
	lager:debug("SmsRequest was sucessfully encoded", []),
	ok = publish_sms_request(Bin, ReqID, GtwID),
	{ok, ReqID, Rejected}.

flash(false, _) ->
	[];
flash(true, default) ->
	[?just_sms_request_param(<<"data_coding">>, 240)];
flash(true, ucs2) ->
	[?just_sms_request_param(<<"data_coding">>, 248)].

publish_sms_request(Payload, ReqID, GtwID) ->
	gen_server:call(?MODULE, {publish, Payload, ReqID, GtwID}).

get_suitable_gtw(Customer, NumberOfDests) ->
	#k1api_auth_response_dto{
		default_provider_id = DefaultProviderID,
		providers = Providers,
		networks = Networks
	} = Customer,
	get_suitable_gtw(DefaultProviderID, Networks, Providers, NumberOfDests).
get_suitable_gtw(undefined, _Networks, _Providers, _NumberOfDests) ->
	erlang:error(gtw_choice_not_implemented);
get_suitable_gtw(DefaultProviderID, _Networks, Providers, _NumberOfDests) ->
	[Provider] = lists:filter(fun(Provider) ->
		Provider#provider_dto.id == DefaultProviderID
		end, Providers),
	Provider#provider_dto.gateway.

get_ids(CustomerID, NumberOfDests, Parts) ->
	{ok, IDs} = soap_db:next_id(CustomerID, NumberOfDests * Parts),
	StringIDs = [integer_to_list(ID) || ID <- IDs],
	{DTOIDs, []} =
		lists:foldl(
		  fun	(ID, {Acc, Group}) when (length(Group) + 1) =:= Parts ->
					GroupIDs = list_to_binary(string:join(lists:reverse([ID | Group]), ":")),
				  	{[GroupIDs | Acc], []};
				(ID, {Acc, Group}) ->
				  	{Acc, [ID | Group]}
		  end, {[], []}, StringIDs),
	DTOIDs.

get_message_parts(Size, default) when Size =< 160 ->
	{ok, 1};
get_message_parts(Size, default) ->
	case (Size rem 153) == 0 of
		true -> {ok, trunc(Size/153)};
		false -> {ok, trunc(Size/153) +1}
	end;
get_message_parts(Size, ucs2) when Size =< 70 ->
	{ok, 1};
get_message_parts(Size, ucs2) ->
	case (Size rem 67) == 0 of
		true -> {ok, trunc(Size/67)};
		false -> {ok, trunc(Size/67) + 1}
	end.

fmt_validity(SecondsTotal) ->
    MinutesTotal = SecondsTotal div 60,
    HoursTotal = MinutesTotal div 60,
    DaysTotal = HoursTotal div 24,
    MonthsTotal = DaysTotal div 30,
    Years = MonthsTotal div 12,
    Seconds = SecondsTotal rem 60,
    Minutes = MinutesTotal rem 60,
    Hours = HoursTotal rem 24,
    Days = DaysTotal rem 30,
    Months = MonthsTotal rem 12,
    StringValidity =
		lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w000R",
                  [Years, Months, Days, Hours, Minutes, Seconds])),
	list_to_binary(StringValidity).

-spec get_allowed_destinations([binary()], [#network_dto{}]) ->
	{Allowed :: binary(), Rejected :: binary()}.
get_allowed_destinations(Destinations, Networks) ->
	get_allowed_destinations(Destinations, Networks, [], []).

get_allowed_destinations([], _Networks, Allowed, Rejected) ->
	{Allowed, Rejected};
get_allowed_destinations([Addr | Rest], Networks, Allowed, Rejected) ->
	case is_addr_allowed(Addr, Networks) of
		true -> get_allowed_destinations(Rest, Networks, [Addr | Allowed], Rejected);
		false -> get_allowed_destinations(Rest, Networks, Allowed, [Addr | Rejected])
	end.

is_addr_allowed(_Addr, []) ->
	false;
is_addr_allowed(Addr, [Network | Rest]) ->
	CountryCode = Network#network_dto.country_code,
	Length = Network#network_dto.numbers_len,
	PrefixesWithCountryCode =
	[<<CountryCode/binary, Prefix/binary>> || Prefix <- Network#network_dto.prefixes],
	case is_addr_allowed(Addr, Length, PrefixesWithCountryCode) of
		true -> true;
		false -> is_addr_allowed(Addr, Rest)
	end.

is_addr_allowed(_Addr, _Length, []) ->
	false;
is_addr_allowed(Addr, Length, [FullPrefix | Rest]) when
				size(Addr#addr.addr) =:= Length ->
	case binary:match(Addr#addr.addr, FullPrefix) of
		{0, _} -> true;
		_ -> is_addr_allowed(Addr, Length, Rest)
	end;
is_addr_allowed(_Addr, _Length, _FullPrefixes) ->
	false.

confirm_select(Channel) ->
    Method = #'confirm.select'{},
    #'confirm.select_ok'{} = amqp_channel:call(Channel, Method),
    ok.
