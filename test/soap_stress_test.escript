#!/usr/bin/env escript

-export([main/1]).

-define(proc_num, 1).
-define(req_num, 1).
-define(batch_size, 2).
-define(req_delay, 0).
-define(start_phone_num, 375259770000).
-define(host, "127.0.0.1").
-define(port, "8088").
-define(soap_uri, "http://" ++ ?host ++ ":" ++ ?port ++ "/bmsgw/soap/messenger.asmx").
-define(urlencoded_uri, "http://" ++ ?host ++ ":" ++ ?port ++ "/bmsgw/soap/messenger.asmx/HTTP_SendSms").
-define(customer, <<"soap-postpaid">>).
-define(user, <<"user">>).
-define(pass, <<"password">>).
-define(originator, <<"999">>).
-define(text, <<"Hello PowerMeMobile!">>).
-define(req_type, post_urlencoded). %% soap11 | soap12 | get_urlencoded | post_urlencoded


%% ===================================================================
%% supervisor section
%% ===================================================================

-spec main(list()) -> no_return().
main(_) ->
    StartTime = now(),
    process_flag(trap_exit, true),
    io:format("Generate ~p recepients...~n",[?batch_size]),
    Recipients =
        list_to_binary(
            string:join([integer_to_list(R) || R <- lists:seq(?start_phone_num, ?start_phone_num + ?batch_size - 1)], ",")),
    io:format("Recipients ready~n",[]),
    ok = inets:start(),
    io:format("Spawn ~p processes...~n", [?proc_num]),
    Workers =
        [spawn_link(fun() -> load_soap_interface(N, Recipients) end) || N <- lists:seq(1, ?proc_num)],
    io:format("Waiting workers...~n", []),
    wait_workers(Workers, StartTime).

wait_workers([], StartTime) ->
    FinishTime = now(),
    TotalTime = timer:now_diff(FinishTime, StartTime)/1000000,
    TotalMsgs = ?proc_num * ?req_num * ?batch_size,
    AvgThroughput = TotalMsgs/TotalTime,
    io:format(
        "~nSummary:~n"
        "Proc num: ~p~n"
        "Req num per proc: ~p~n"
        "Batch size (recipients per sms req): ~p~n"
        "Total msgs: ~p~n"
        "Total time: ~p~n"
        "Average throughput: ~p~n~n",
            [?proc_num, ?req_num, ?batch_size, TotalMsgs, TotalTime, AvgThroughput]),
    erlang:halt();
wait_workers(Workers, StartTime) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("~p terminated with ~p~n", [Pid, Reason]),
            wait_workers(Workers -- [Pid], StartTime);
        Msg ->
            io:format("Sup got unexpected msg: ~p~n. Halt.", [Msg]),
            erlang:halt()
    end.


%% ===================================================================
%% worker section
%% ===================================================================

load_soap_interface(_N, Recipients) ->
    io:format("~p Started ~n",[self()]),
    {Method, URI, Headers, ContentType, Body, Queries} =
    case ?req_type of
        soap12 ->
            {post, ?soap_uri, [], "", soap12(Recipients), []};
        soap11 ->
            {post, ?soap_uri, [], "", soap11(Recipients), []};
        get_urlencoded ->
            Q = [{"customerID",?customer},
                {"userName", ?user},
                {"userPassword", ?pass},
                {"originator", ?originator},
                {"smsText", ?text},
                {"recipientPhone", Recipients},
                {"messageType", "Latin"},
                {"defDate", ""},
                {"blink", "false"},
                {"flash", "false"},
                {"Private", "false"}],
            {get, ?urlencoded_uri, [], "", [], "?" ++ query_string(Q)};
        post_urlencoded ->
            Q = [{"customerID",?customer},
                {"userName", ?user},
                {"userPassword", ?pass},
                {"originator", ?originator},
                {"smsText", ?text},
                {"recipientPhone", Recipients},
                {"messageType", "Latin"},
                {"defDate", ""},
                {"blink", "false"},
                {"flash", "false"},
                {"Private", "false"}],
            CT = "application/x-www-form-urlencoded",
            {post, ?urlencoded_uri, [], CT, query_string(Q), []} % <- encode body for post request
    end,
    perform_request(Method, URI, Headers, ContentType, Body, Queries, ?req_num).


perform_request(_Method, _Url, _Headers, _ContentType, _Body, _Queries, 0) ->
    io:format("~p finished~n",[self()]);
perform_request(Method, Url, Headers, ContentType, Body, Queries, Num) ->
    {ok, _Status} = perform_request({Method, Url, Headers, ContentType, Queries, Body}),
    timer:sleep(?req_delay),
    perform_request(Method, Url, Headers, ContentType, Body, Queries, Num-1).

perform_request({Method, Url, Headers, ContentType, Queries, Body}) ->
    FullUrl = Url ++ Queries,
    Request = case Method of
        get -> {FullUrl, Headers};
        delete -> {FullUrl, Headers};
        _   -> {FullUrl, Headers, ContentType, Body}
    end,
    {ok, {{_, StatusCode, _}, _ResHeaders, _ResBody}} =
        httpc:request(Method, Request, [], []),
    {ok, StatusCode}.


query_string([Head|Tail]) ->
    lists:flatten([make_query(Head) | [["&", make_query(Elem)] || Elem <- Tail]]);
query_string([]) -> [].


make_query({Key, Value}) ->
    [url_encode(Key), "=", url_encode(Value)].

url_encode(Value) when is_list(Value) ->
    http_uri:encode(Value);
url_encode(Value) when is_bitstring(Value) ->
    url_encode(binary_to_list(Value));
url_encode(Value) when is_integer(Value) ->
    Value.

soap11(Recipients) ->
    SoapBody = <<"<soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/' xmlns:pmm='http://pmmsoapmessenger.com/'><soapenv:Header/><soapenv:Body><pmm:HTTP_SendSms><pmm:customerID>%customer%</pmm:customerID><pmm:userName>%user%</pmm:userName><pmm:userPassword>%password%</pmm:userPassword><pmm:originator>%originator%</pmm:originator><pmm:smsText>%text%</pmm:smsText><pmm:recipientPhone>%recipients%</pmm:recipientPhone><pmm:messageType>ArabicWithLatinNumbers</pmm:messageType><pmm:defDate></pmm:defDate><pmm:blink>false</pmm:blink><pmm:flash>false</pmm:flash><pmm:Private>false</pmm:Private></pmm:HTTP_SendSms></soapenv:Body></soapenv:Envelope>">>,
    perform_soap(SoapBody, Recipients).


soap12(Recipients) ->
    SoapBody = <<"<?xml version='1.0' encoding='UTF-8'?><soapenv:Envelope xmlns:soapenv='http://www.w3.org/2003/05/soap-envelope'><soapenv:Body><ns1:SendSms xmlns:ns1='http://pmmsoapmessenger.com/'><ns1:user><ns1:CustomerID>%customer%</ns1:CustomerID><ns1:Name>%user%</ns1:Name><ns1:Language>0</ns1:Language><ns1:Password>%password%</ns1:Password></ns1:user><ns1:originator>%originator%</ns1:originator><ns1:smsText>%text%</ns1:smsText><ns1:recipientPhone>%recipients%</ns1:recipientPhone><ns1:messageType>Latin</ns1:messageType><ns1:blink>false</ns1:blink><ns1:flash>false</ns1:flash><ns1:Private>false</ns1:Private></ns1:SendSms></soapenv:Body></soapenv:Envelope>">>,
    perform_soap(SoapBody, Recipients).

perform_soap(SoapBody, Recipients) ->
    SoapBody1 = binary:replace(SoapBody, <<"%customer%">>, ?customer),
    SoapBody2 = binary:replace(SoapBody1, <<"%user%">>, ?user),
    SoapBody3 = binary:replace(SoapBody2, <<"%password%">>, ?pass),
    SoapBody4 = binary:replace(SoapBody3, <<"%originator%">>, ?originator),
    SoapBody5 = binary:replace(SoapBody4, <<"%recipients%">>, Recipients),
    SoapBody6 = binary:replace(SoapBody5, <<"%text%">>, ?text),
    binary_to_list(SoapBody6).
