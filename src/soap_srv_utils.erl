-module(soap_srv_utils).

-export([
    addr_to_dto/1
]).

-include_lib("alley_dto/include/adto.hrl").
-include("soap_srv.hrl").

-define(TON_UNKNOWN,       0).
-define(TON_INTERNATIONAL, 1).
-define(TON_NATIONAL,      2).
-define(TON_ALPHANUMERIC,  5).
-define(TON_ABBREVIATED,   6).

-define(NPI_UNKNOWN,       0).
-define(NPI_ISDN,          1). % E163/E164

%% ===================================================================
%% Addr to dto
%% ===================================================================

-spec addr_to_dto(Addr :: binary()) -> #addr{}.
addr_to_dto(AddrBin) when is_binary(AddrBin) ->
    Addr = binary_to_list(AddrBin),
    IsInteger =
        try list_to_integer(Addr) of
            _ -> true
        catch
            _:_ -> false
        end,
    Length = length(Addr),
    addr_to_dto(AddrBin, IsInteger, Length).

addr_to_dto(AddrBin, true, Length) when Length < 7 -> % 1..6
    #addr{
        addr = AddrBin,
        ton = ?TON_ABBREVIATED,
        npi = ?NPI_UNKNOWN
    };
addr_to_dto(AddrBin, true, _Length) -> % 7..
    #addr{
        addr = AddrBin,
        ton = ?TON_INTERNATIONAL,
        npi = ?NPI_ISDN
    };
addr_to_dto(AddrBin, false, _Length) ->
    #addr{
        addr = AddrBin,
        ton = ?TON_ALPHANUMERIC,
        npi = ?NPI_UNKNOWN
    }.
