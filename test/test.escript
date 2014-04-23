#!/usr/bin/env escript

-export([main/1]).

-define(gv(Key, PropList), proplists:get_value(Key, PropList)).
-define(gv(Key, PropList, Default), proplists:get_value(Key, PropList, Default)).

-spec main(list()) -> no_return().
main(Args) ->
    process_flag(trap_exit, true),
    {ok, CWD} = file:get_cwd(),
    io:format("[debug] pwd: ~p~n", [CWD]),
    io:format("Start inets~n", []),
    ok = inets:start(),
    io:format("Read config~n", []),
    {ok, [Config]} =
        case lists:suffix("test", CWD) of
            true ->
                file:consult(filename:join(CWD, "soap_test.config"));
            false ->
                file:consult(filename:join(CWD, "test/soap_test.config"))
            end,
    Props = ?gv(props, Config),

    TestsToRun =
    case Args of
        [TestName] ->
            %% name of test defined, look for it in config
            io:format("Run ~p~n", [TestName]),
            [T || T <- ?gv(tests, Config),
                TestName =:= proplists:get_value(name, T)];
        _ ->
            %% name of test UNdefined, run all tests
            AllTests = ?gv(tests, Config),
            io:format("Run all test (~p)~n", [length(AllTests)]),
            AllTests
    end,

    %% construct recipient list
    StartDestAddr = ?gv(dest_addr, Props),
    IntRecipientList = lists:seq(StartDestAddr, StartDestAddr + 1),
    StrRecipientList = [integer_to_list(R) || R <- IntRecipientList],
    Recipients = list_to_binary( string:join(StrRecipientList, ",")),
    io:format("Recipients: ~p~n", [Recipients]),
    Base64Recipients = base64:encode(Recipients),
    io:format("Base64 encoded recipients: ~p~n", [Base64Recipients]),
    FullProps = [{base64recipients, Base64Recipients}, {recipient, Recipients}] ++ Props,
    do_req(TestsToRun, FullProps).

do_req([], _Props) ->
    io:format("All done~n", []),
    erlang:halt(0);
do_req([Test | Rest], Props) ->
    io:format("Start ~p~n", [?gv(name, Test)]),
    Req = perform_req(?gv(req, Test, <<>>), Props),
    Queries = [{K, perform_req(V, Props)} || {K, V} <- ?gv('query', Test, "")],
    io:format("ReqBody: ~p~n", [Req]),
    io:format("ReqQueries: ~p~n", [Queries]),
    Method = ?gv(http_method, Test),
    Url = ?gv(url, Props) ++ ?gv(soap_method, Test, ""),
    Headers = ?gv(headers, Test, []),
    io:format("ReqHeaders: ~p~n", [Headers]),
    ContentType = ?gv('content-type', Test),
    {ok, 200} = perform_request({Method, Url, Headers, ContentType, Queries, Req}),
    io:format("Success~n~n", []),
    do_req(Rest, Props).

perform_request({Method, Url, Headers, ContentType, Queries, Body}) ->
    FullUrl = Url ++ query_string(Queries),
    io:format("FullUrl: ~p~n", [FullUrl]),
    Request = case Method of
        get -> {FullUrl, Headers};
        delete -> {FullUrl, Headers};
        _   -> {FullUrl, Headers, ContentType, Body}
    end,
    {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} =
            httpc:request(Method, Request, [], []),
    io:format("RespHeaders: ~p~n", [RespHeaders]),
    io:format("RespBody: ~p~n", [RespBody]),
    {ok, StatusCode}.

perform_req(Req, Proplist) ->
    Fun =
    fun
        ({Key, Value}, ReqAcc) when is_binary(Value) ->
            binary:replace(ReqAcc, placeholder(Key), Value);
        (_, ReqAcc) -> ReqAcc
    end,
    lists:foldr(Fun, Req, Proplist).

placeholder(Key) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    <<$%,BinKey/binary,$%>>.

query_string([Head|Tail]) ->
    lists:flatten(["?" ++ make_query(Head) | [["&", make_query(Elem)] || Elem <- Tail]]);
query_string([]) -> [].


make_query({Key, Value}) ->
    [url_encode(Key), "=", url_encode(Value)].

url_encode(Value) when is_list(Value) ->
    http_uri:encode(Value);
url_encode(Value) when is_bitstring(Value) ->
    url_encode(binary_to_list(Value));
url_encode(Value) when is_integer(Value) ->
    Value.