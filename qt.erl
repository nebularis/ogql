#!/usr/bin/env escript
%%! -pa ebin
-module(qt).
-compile(export_all).

main(_) ->
    run_query("platformSystem"),
    run_query("platformSystem,systemConsumingPlatform"),
    run_query("service-interface").

run_query(Query) ->
    io:format("~s => ~p~n", [Query, ogql_grammar:parse(Query)]).
