-module(soap_srv_auth).

-ignore_xref([{start_link, 0}]).

%% API
-export([
    start_link/0,
    authenticate/3
]).

-include("logging.hrl").
-include("application.hrl").
-include_lib("alley_dto/include/adto.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, QueueName} = application:get_env(?APP, kelly_auth_queue),
    rmql_rpc_client:start_link(?MODULE, QueueName).

-spec authenticate(binary(), binary(), binary()) ->
    {ok, #k1api_auth_response_dto{}} |
    {error, timeout}.
authenticate(CustomerId, UserId, Password) ->
    authenticate(check_cache, CustomerId, UserId, Password).

%% ===================================================================
%% Internal
%% ===================================================================

authenticate(check_cache, CustomerId, UserId, Password) ->
    case soap_srv_auth_cache:fetch(CustomerId, UserId) of
        {ok, AuthResp} ->
            {ok, AuthResp};
        not_found ->
            authenticate(request_backend, CustomerId, UserId, Password)
    end;

authenticate(request_backend, CustomerId, UserId, Password) ->
    ReqId = uuid:unparse(uuid:generate_time()),
    AuthReq = #k1api_auth_request_dto{
        id = ReqId,
        customer_id = CustomerId,
        user_id = UserId,
        password = Password,
        connection_type = soap
    },
    ?log_debug("Sending auth request: ~p", [AuthReq]),
    {ok, Payload} = adto:encode(AuthReq),
    case rmql_rpc_client:call(?MODULE, Payload, <<"OneAPIAuthReq">>) of
        {ok, Bin} ->
            case adto:decode(#k1api_auth_response_dto{}, Bin) of
                {ok, AuthResp = #k1api_auth_response_dto{result = Result}} ->
                    ?log_debug("Got auth response: ~p", [AuthResp]),
                    case Result of
                        {customer, _} ->
                            ok = soap_srv_auth_cache:store(CustomerId, UserId, AuthResp);
                        {error, _} ->
                            ok
                    end,
                    {ok, AuthResp};
                {error, Error} ->
                    ?log_error("Auth response decode error: ~p", [Error]),
                    {error, Error}
            end;
        {error, timeout} ->
            ?log_debug("Got auth response: timeout", []),
            {error, timeout}
    end.
