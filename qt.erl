#!/usr/bin/env escript
%%! -pa ebin -pa deps/dh_date/ebin -pa deps/semver/ebin
-module(qt).
-compile(export_all).

main(["demo"]) ->
    run_query("?[::$(version.major) gt 10 AND
                 ::$(lastmodified.user) eq 991726352]");
main([Expr]) when is_list(Expr) ->
    run_query(Expr);
main(_) ->
    run_query("a-b"),
    run_query("a => b"),
    run_query("a => b => c"),
    %% note that this is equivalent to
    %% a => (b => (c => d))
    run_query("a => b => c => d"),
    run_query("a => (b => (c => d))"),
    run_query("(a-b => b-c)"),
    run_query("((a-b => (b-c, b-d)) => (c-e, d-e) => e-f)"),
    run_query("((a-b <- (b-c, b-d)) => (b-k, b-n) => n-z)"),
    run_query("flow-step => (step-ifc <- (ifc-operations), step-appcomp) => ifc-service"),
    run_query("((design-flow <- (flow-step => step-ifc => ifc-op)) => !service-flow)"),
    run_query("flow-step => ((step-ifc <- ifc-operations), step-appcomp) => ifc-service"),
    run_query("svc-flow[provider::$(key) eq 108 AND consumer::flow-type eq 'standard'] => 
                (flow-step => *step-step) <- (step-ifc, (step-appcomp => !platform-appcomp => plat-subPlat), step-org)"),
    run_query("a-b => b-c => c-d"),
    run_query("platformSystem"),
    run_query("platformSystem => systemConsumingPlatform"),
    run_query("service-interface"),
    run_query("service-interface[provider::name eq 'BPP']"),
    run_query("service-interface[consumer::processId starts_with '1349']"),
    run_query("Person => ?[provider::name eq 'Joe']"),
    run_query("consumer-products => Offer"),
    (get_writer())("Spaces in between steps...~n", []),
    run_query("Department[::region eq 'Greater London'] =>
                Employee[::firstName like 'John%']"),
    (get_writer())("Logical conjunctions/disjunctions...~n", []),
    run_query("Person[::post-code starts_with 'SE9' AND ::name like 'Joe']"),
    run_query("Person[::post-code starts_with 'SE9' AND "
                                "::name like 'Joe' OR "
                                "::contact_details contains 'Besborough']"),
    run_query("Person[::name like 'Joe' AND ::age gt 18.5]"),
    run_query("Person[::date-of-birth gt DATE(21-3-1972)]"),
    run_query("Service[::is-daemon eq TRUE OR ::active eq FALSE]"),
    run_query("Service[::classification eq :strategic]"),
    (get_writer())("Filter predicates on `internal' fields...~n", []),
    run_query("?[::$(name) eq 'Caller' AND 
                            ::$(description) contains 'APMO']"),
    run_query("?[::$(version.major) gt 10 AND 
                 ::$(lastmodified.user) eq 991726352]"),
    run_query("?[::$(version.major) eq 2 AND 
                 ::$(version.minor) eq 0 AND 
                 ::$(version.build) eq 1]"),
    run_query("?[::$(version) gt VSN(1.6.13-RC2)]"),
    (get_writer())("Examples of grouping...~n", []),
    run_query("a-b => (b-c, b-d)"),
    run_query("server-interface =>
              (interface-client[consumer::classification eq 'STRATEGIC'], interface-api)"),
    run_query("server-interface =>
                interface-client[::group eq :b2b] =>
                    client-sla =>
                        (sla-groups[::$(key) eq :b2b], sla-documents)"),
    run_query("ancestry-person => *person-person"),
    run_query("platform-system =>
            *((system-system, system-interface) => interface-system)"),
    run_query("*(personRoles => roleRelationship)").

run_query(Query) ->
    write_ast(Query, ogql:parse(Query)).
    
write_ast(Query, AST) when is_tuple(AST) ->
    (get_writer())("~s => ~p~n", [Query, AST]).

get_writer() ->
    Writer = case get(writer) of
        undefined -> fun io:format/2;
        W when is_function(W) -> W
    end.
