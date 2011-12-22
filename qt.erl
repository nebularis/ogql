#!/usr/bin/env escript
%%! -pa ebin -pa deps/dh_date/ebin -pa deps/semver/ebin
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
                                "::contact_details contains 'Besborough']"),
    run_query("Person[::name like 'Joe' AND ::age > 18.5]"),
    run_query("Person[::date-of-birth > DATE(21-3-1972)]"),
    run_query("?[::$(name) = 'Caller' AND 
                            ::$(description) contains 'APMO']"),
    run_query("?[::$(version.major) > 10 AND 
                 ::$(lastmodified.user) = 991726352]"),
    run_query("?[::$(version.major) = 2 AND 
                 ::$(version.minor) = 0 AND 
                 ::$(version.build) = 1]"),
    run_query("?[::$(version) > VSN(1.6.13-RC2)]").

run_query(Query) ->
    io:format("~s => ~p~n", [Query, ogql_grammar:parse(Query)]).
