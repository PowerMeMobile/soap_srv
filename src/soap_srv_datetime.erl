-module(soap_srv_datetime).

%% API
-export([
    datetime_to_timestamp/1
]).

-define(GREGORIAN_SECS_BEFORE_UNIX_EPOCH, 62167219200).

-type datetime() :: calendar:datetime().
-type timestamp() :: erlang:timestamp().
-type unixepoch() :: pos_integer().

%% ===================================================================
%% API
%% ===================================================================

-spec datetime_to_timestamp(datetime()) -> timestamp().
datetime_to_timestamp(Datetime) ->
    UnixEpoch = calendar:datetime_to_gregorian_seconds(Datetime) - ?GREGORIAN_SECS_BEFORE_UNIX_EPOCH,
    unixepoch_to_timestamp(UnixEpoch).

-spec unixepoch_to_timestamp(unixepoch()) -> timestamp().
unixepoch_to_timestamp(UnixEpoch) ->
    M = UnixEpoch div 1000000,
    S = UnixEpoch rem 1000000,
    {M, S, 0}.
