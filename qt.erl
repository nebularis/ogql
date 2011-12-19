#!/usr/bin/env escript
%%! -pa ebin
-module(qt).
-compile(export_all).

main(_) ->
    run_query("a-b,b-c,c-d"),
    run_query("platformSystem"),
    run_query("platformSystem,systemConsumingPlatform"),
    run_query("service-interface"),
    run_query("service-interface[provider::name = 'BPP']"),
    run_query("service-interface[consumer::processId starts_with '1349']"),
    run_query("Person,?[provider::name='Joe']"),
    run_query("consumer-products,Offer"),
    run_query("Department[::region = 'Greater London'],
                Employee[::firstName like 'John%']"),
    run_query("Person[::post-code starts_with 'SE9' AND ::name like 'Joe']"),
    run_query("Person[::post-code starts_with 'SE9' AND "
                                "::name like 'Joe' OR "
                                "::contact_details contains 'Besborough']").

run_query(Query) ->
    io:format("~s => ~p~n", [Query, ogql_grammar:parse(Query)]).
