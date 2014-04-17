-module(soap_srv_mt).

%% TODO
%% Disable receipts in Kelly
%% Implement get_suitable_gtw

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

%% API
-export([
    start_link/0,
    send/1,
    publish/1
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

-include("logging.hrl").
-include("soap_srv.hrl").
-include("application.hrl").
-include_lib("alley_dto/include/adto.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

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
    id      :: integer(),
    from    :: term()
}).

-record(st, {
    chan            :: pid(),
    chan_mon_ref    :: reference(),
    next_id = 1     :: integer()
}).

-type payload() :: binary().
-type publish_action() ::
    publish |
    publish_kelly |
    publish_just.
-type req_id() :: binary().
-type gtw_id() :: binary().

%% ===================================================================
%% API Functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(#send_req{}) -> {ok, [{K::atom(), V::any()}]}.
send(Req) ->
    send(authenticate, Req).

-spec publish({publish_action(), payload(), req_id(), gtw_id()}) -> ok.
publish(Req) ->
    gen_server:call(?MODULE, Req).

%% ===================================================================
%% GenServer Callback Functions Definitions
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table, ordered_set, {keypos, 2}]),
    case setup_chan(#st{}) of
        {ok, St} ->
            ?log_info("mt_srv: started", []),
            {ok, St};
        unavailable ->
            ?log_error("mt_srv: initializing failed (amqp_unavailable). shutdown", []),
            {stop, amqp_unavailable}
    end.

handle_call({Action, Payload, ReqID, GtwID}, From, St = #st{}) when
        Action =:= publish orelse
        Action =:= publish_kelly orelse
        Action =:= publish_just ->
    {ok, SmsRequestQueue} = application:get_env(?APP, kelly_sms_request_queue),
    {ok, GtwQueueFmt} = application:get_env(?APP, just_gateway_queue_fmt),
    GtwQueue = binary:replace(GtwQueueFmt, <<"%id%">>, GtwID),

    %% use rabbitMQ 'CC' extention to avoid double publish confirm per 1 request
    {Headers, RoutinKey} =
        case Action of
            publish ->
                CC = {<<"CC">>, array, [{longstr, GtwQueue}]},
                {[CC], SmsRequestQueue};
            publish_kelly ->
                {[], SmsRequestQueue};
            publish_just ->
                {[], GtwQueue}
        end,
    Props = [
        {content_type, <<"OneAPISmsRequest">>},
        {delivery_mode, 2},
        {priority, 1},
        {message_id, ReqID},
        {headers, Headers}
    ],
    Channel = St#st.chan,
    ok = rmql:basic_publish(Channel, RoutinKey, Payload, Props),
    true = ets:insert(?MODULE, #unconfirmed{id = St#st.next_id, from = From}),
    {noreply, St#st{next_id = St#st.next_id + 1}};

handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(Req, St) ->
    {stop, {unexpected_cast, Req}, St}.

handle_info(#'DOWN'{ref = Ref, info = Info}, St = #st{chan_mon_ref = Ref}) ->
    ?log_error("mt_srv: amqp channel down (~p)", [Info]),
    {stop, amqp_channel_down, St};

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
%% Send steps
%% ===================================================================

send(authenticate, Req) ->
    CustomerID = Req#send_req.customer_id,
    UserName = Req#send_req.user_name,
    Pass = Req#send_req.password,
    case soap_srv_auth:authenticate(CustomerID, UserName, Pass) of
        {ok, #k1api_auth_response_dto{result = {customer, Customer}}} ->
            Req2 = Req#send_req{customer = Customer},
            send(perform_src_addr, Req2);
        {ok, #k1api_auth_response_dto{result = {error, Error}}} ->
            {ok, [{result, Error}]};
        {error, timeout} ->
            {ok, [{result, ?authError}]}
    end;

send(perform_src_addr, Req) ->
    Customer = Req#send_req.customer,
    Originator = soap_srv_utils:addr_to_dto(Req#send_req.originator),
    AllowedSources = Customer#k1api_auth_response_customer_dto.allowed_sources,
    case lists:member(Originator, AllowedSources) of
        true ->
            send(perform_dest_addr, Req#send_req{originator = Originator});
        false ->
            {ok, [{result, ?originatorNotAllowedError}]}
    end;

send(perform_dest_addr, Req = #send_req{}) ->
    Customer = Req#send_req.customer,
    Networks = Customer#k1api_auth_response_customer_dto.networks,
    BlobRecipients = Req#send_req.recipients,
    RawRecipients = binary:split(BlobRecipients, <<",">>, [trim, global]),
    case get_allowed_destinations(RawRecipients, Networks) of
        {[], _} -> {ok, [{result, ?noAnyDestAddrError}]};
        {Recipients, Rejected} ->
            send(process_msg_type, Req#send_req{recipients = Recipients, rejected = Rejected})
    end;

send(process_msg_type, Req) when Req#send_req.text =:= undefined andalso
                                 Req#send_req.action =:= 'SendServiceSms' ->
    Text =
        <<"<%SERVICEMESSAGE:",
        (Req#send_req.s_name)/binary, ";",
        (Req#send_req.s_url)/binary, "%>">>,
    send(process_msg_type, Req#send_req{text = Text});

send(process_msg_type, Req) when Req#send_req.text =:= undefined andalso (
                                 Req#send_req.action =:= 'SendBinarySms' orelse
                                 Req#send_req.action =:= 'HTTP_SendBinarySms') ->
    Text = hexstr_to_bin(binary_to_list(Req#send_req.binary_body)),
    send(define_smpp_params, Req#send_req{text = Text, encoding = default, encoded = <<" ">>});

send(process_msg_type, Req) ->
    Text = convert_numbers(Req#send_req.text, Req#send_req.type),
    send(define_text_encoding, Req#send_req{text = Text});

send(define_text_encoding, Req) ->
    {Encoding, Encoded} =
        case gsm0338:from_utf8(Req#send_req.text) of
            {valid, Binary} -> {default, Binary};
            {invalid, Binary} -> {ucs2, Binary}
        end,
    send(define_smpp_params, Req#send_req{encoding = Encoding, encoded = Encoded});

send(define_smpp_params, Req) when Req#send_req.action =:= 'SendServiceSms' ->
    Customer = Req#send_req.customer,
    NoRetry = Customer#k1api_auth_response_customer_dto.no_retry,
    DefaultValidity = Customer#k1api_auth_response_customer_dto.default_validity,
    Params = lists:flatten([
        ?just_sms_request_param(<<"registered_delivery">>, true),
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
    send(build_dto, Req#send_req{smpp_params = Params});

send(define_smpp_params, Req) when Req#send_req.action =:= 'SendBinarySms' orelse
                                    Req#send_req.action =:= 'HTTP_SendBinarySms' ->
    Customer = Req#send_req.customer,
    NoRetry = Customer#k1api_auth_response_customer_dto.no_retry,
    DefaultValidity = Customer#k1api_auth_response_customer_dto.default_validity,
    _DC = list_to_integer(binary_to_list(Req#send_req.data_coding)),
    ESMClass = list_to_integer(binary_to_list(Req#send_req.esm_class)),
    ProtocolID = list_to_integer(binary_to_list(Req#send_req.protocol_id)),
    Params = lists:flatten([
        ?just_sms_request_param(<<"registered_delivery">>, true),
        ?just_sms_request_param(<<"service_type">>, <<>>),
        ?just_sms_request_param(<<"no_retry">>, NoRetry),
        ?just_sms_request_param(<<"validity_period">>, fmt_validity(DefaultValidity)),
        ?just_sms_request_param(<<"priority_flag">>, 0),
        ?just_sms_request_param(<<"esm_class">>, ESMClass),
        ?just_sms_request_param(<<"protocol_id">>, ProtocolID)
        %% ?just_sms_request_param(<<"data_coding">>, SendBinarySmsReq#send_binary_sms_req.data_coding)
     ]),
    send(build_dto, Req#send_req{smpp_params = Params});

send(define_smpp_params, Req) ->
    Encoding = Req#send_req.encoding,
    Customer = Req#send_req.customer,
    NoRetry = Customer#k1api_auth_response_customer_dto.no_retry,
    DefaultValidity = Customer#k1api_auth_response_customer_dto.default_validity,
    Params = lists:flatten([
        ?just_sms_request_param(<<"registered_delivery">>, true),
        ?just_sms_request_param(<<"service_type">>, <<>>),
        ?just_sms_request_param(<<"no_retry">>, NoRetry),
        ?just_sms_request_param(<<"validity_period">>, fmt_validity(DefaultValidity)),
        ?just_sms_request_param(<<"priority_flag">>, 0),
        ?just_sms_request_param(<<"esm_class">>, 3),
        ?just_sms_request_param(<<"protocol_id">>, 0)
    ]) ++ flash(get_boolean(Req#send_req.flash), Encoding),
    send(build_dto, Req#send_req{smpp_params = Params});

send(build_dto, Req) ->
    Encoding = Req#send_req.encoding,
    Encoded = Req#send_req.encoded,
    NumberOfSymbols = size(Encoded),
    {ok, NumberOfParts} = get_message_parts(NumberOfSymbols, Encoding),
    Customer = Req#send_req.customer,
    CustomerID = Customer#k1api_auth_response_customer_dto.uuid,
    UserID = Req#send_req.user_name,
    ReqID = uuid:unparse(uuid:generate_time()),
    Destinations = Req#send_req.recipients,
    NumberOfDests = length(Destinations),
    GtwID = get_suitable_gtw(Req#send_req.customer, NumberOfDests),
    MessageIDs = get_ids(CustomerID, UserID, NumberOfDests, NumberOfParts),
    DTO = #just_sms_request_dto{
        id = ReqID,
        gateway_id = GtwID,
        customer_id = CustomerID,
        user_id = Req#send_req.user_name,
        client_type = soap,
        type = regular,
        message = Req#send_req.text,
        encoding = Encoding,
        params = Req#send_req.smpp_params,
        source_addr = Req#send_req.originator,
        dest_addrs = {regular, Destinations},
        message_ids = MessageIDs
    },
    ?log_debug("Sending submit request: ~p", [DTO]),
    {ok, Payload} = adto:encode(DTO),
    case is_deferred(Req#send_req.def_date) of
        {true, Timestamp} ->
            ?log_info("mt_srv: defDate -> ~p, timestamp -> ~p", [Req#send_req.def_date, Timestamp]),
            soap_srv_defer:defer(ReqID, Timestamp, {publish_just, Payload, ReqID, GtwID}),
            ok = publish({publish_kelly, Payload, ReqID, GtwID}),
            soap_srv_pdu_logger:log(DTO),
            {ok, [{id,ReqID}, {rejected, Req#send_req.rejected}]};
        false ->
            ok = publish({publish, Payload, ReqID, GtwID}),
            soap_srv_pdu_logger:log(DTO),
            {ok, [{id,ReqID}, {rejected, Req#send_req.rejected}]};
        {error, invalid} ->
            {ok, [{result, ?invalidDefDateFormatError}]}
    end.

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

setup_chan(St = #st{}) ->
    {ok, SmsRequestQueue} = application:get_env(?APP, kelly_sms_request_queue),
    case rmql:channel_open() of
        {ok, Channel} ->
            ChanMonRef = erlang:monitor(process, Channel),
            amqp_channel:register_confirm_handler(Channel, self()),
            #'confirm.select_ok'{} = amqp_channel:call(Channel, #'confirm.select'{}),
            ok = rmql:queue_declare(Channel, SmsRequestQueue, []),
            {ok, St#st{chan = Channel, chan_mon_ref = ChanMonRef}};
        unavailable -> unavailable
    end.

flash(false, _) ->
    [];
flash(true, default) ->
    [?just_sms_request_param(<<"data_coding">>, 240)];
flash(true, ucs2) ->
    [?just_sms_request_param(<<"data_coding">>, 248)].

get_suitable_gtw(Customer, NumberOfDests) ->
    #k1api_auth_response_customer_dto{
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
    Provider#provider_dto.gateway_id.

get_ids(CustomerID, UserID, NumberOfDests, Parts) ->
    {ok, IDs} = soap_srv_db:next_id(CustomerID, UserID, NumberOfDests * Parts),
    {DTOIDs, []} =
        lists:foldl(
          fun(ID, {Acc, Group}) when (length(Group) + 1) =:= Parts ->
                  StrID = integer_to_list(ID),
                  GroupIDs = list_to_binary(string:join(lists:reverse([StrID | Group]), ":")),
                  {[GroupIDs | Acc], []};
             (ID, {Acc, Group}) ->
                  {Acc, [integer_to_list(ID) | Group]}
          end, {[], []}, IDs),
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
    {Allowed::[#addr{}], Rejected::[binary()]}.
get_allowed_destinations(Destinations, Networks) ->
    get_allowed_destinations(Destinations, Networks, [], []).

get_allowed_destinations([], _Networks, Allowed, Rejected) ->
    {Allowed, Rejected};
get_allowed_destinations([Addr | Rest], Networks, Allowed, Rejected) ->
    case is_addr_allowed(Addr, Networks) of
        true ->
            AddrDTO = soap_srv_utils:addr_to_dto(Addr),
            get_allowed_destinations(Rest, Networks, [AddrDTO | Allowed], Rejected);
        false -> get_allowed_destinations(Rest, Networks, Allowed, [Addr | Rejected])
    end.

is_addr_allowed(_Addr, []) ->
    false;
is_addr_allowed(Addr, [Network | Rest]) ->
    CountryCode = Network#network_dto.country_code,
    NumberLen = Network#network_dto.number_len,
    PrefixesWithCountryCode =
        [<<CountryCode/binary, Prefix/binary>> || Prefix <- Network#network_dto.prefixes],
    case is_addr_allowed(Addr, NumberLen, PrefixesWithCountryCode) of
        true -> true;
        false -> is_addr_allowed(Addr, Rest)
    end.

is_addr_allowed(_Addr, _Length, []) ->
    false;
is_addr_allowed(Addr, Length, [FullPrefix | Rest]) when
                size(Addr) =:= Length ->
    case binary:match(Addr, FullPrefix) of
        {0, _} -> true;
        _ -> is_addr_allowed(Addr, Length, Rest)
    end;
is_addr_allowed(_Addr, _Length, _FullPrefixes) ->
    false.

get_boolean(<<"true">>) -> true;
get_boolean(<<"false">>) -> false.

convert_numbers(Text, <<"ArabicWithArabicNumbers">>) ->
    case unicode:characters_to_list(Text, utf8) of
        CodePoints when is_list(CodePoints) ->
            ConvCP = [number_to_arabic(CP) || CP <- CodePoints],
            unicode:characters_to_binary(ConvCP, utf8);
        {error, CodePoints, RestData} ->
            ?log_error("mt_srv: Arabic numbers to hindi error. Original: ~w Codepoints: ~w Rest: ~w",
                    [Text, CodePoints, RestData]),
            erlang:error("Illegal utf8 symbols");
        {incomplete, CodePoints, IncompleteSeq} ->
            ?log_error("mt_srv: Incomplete utf8 sequence. Original: ~w Codepoints: ~w IncompleteSeq: ~w",
                    [Text, CodePoints, IncompleteSeq]),
            erlang:error("Incomplite utf8 sequence")
    end;
convert_numbers(Text, _) ->
    Text.

number_to_arabic(16#0030) -> 16#0660;
number_to_arabic(16#0031) -> 16#0661;
number_to_arabic(16#0032) -> 16#0662;
number_to_arabic(16#0033) -> 16#0663;
number_to_arabic(16#0034) -> 16#0664;
number_to_arabic(16#0035) -> 16#0665;
number_to_arabic(16#0036) -> 16#0666;
number_to_arabic(16#0037) -> 16#0667;
number_to_arabic(16#0038) -> 16#0668;
number_to_arabic(16#0039) -> 16#0669;
number_to_arabic(Any) -> Any.

%% replace with ac_hexdump:binary/1,2
hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

is_deferred(<<>>) -> false;
is_deferred(undefined) -> false;
is_deferred(DefDateList) when is_list(DefDateList) ->
    try [list_to_integer(binary_to_list(D)) || D <- DefDateList] of
        [Month, Day, Year, Hour, Min] ->
            DateTime = {{Year, Month, Day}, {Hour, Min, 0}},
            {true, ac_datetime:datetime_to_timestamp(DateTime)}
    catch
        _:_ -> {error, invalid}
    end;
is_deferred(DefDate) when is_binary(DefDate) ->
    case binary:split(DefDate, [<<"/">>, <<" ">>, <<":">>], [global]) of
        [_M, _D, _Y, _H, _Min] = DefDateList ->
            is_deferred(DefDateList);
        _ -> {error, invalid}
    end.
