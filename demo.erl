#!/usr/bin/env escript
%%! -pa ebin -pa deps/dh_date/ebin -pa deps/semver/ebin
-module(demo).
-compile(export_all).

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
                (flow-step => *step-step) <- (step-ifc, (step-appcomp => !platform-appcomp => plat-subPlat), step-org)").

run_query(Query) ->
    io:format("~s :: ~p~n", [Query, ogql_grammar:parse(Query)]).

test() ->
    {intersect, {
        {{implicit_name_predicate, "a-b"},
             {subquery,
                  {union, {{implicit_name_predicate, "b-c"},
                           {implicit_name_predicate, "c-d"}}}}},
         {intersect,
          {{union, {{implicit_name_predicate, "b-k"},
                    {implicit_name_predicate, "b-n"}}},
           {implicit_name_predicate,"n-z"}}}}},
    
    {intersect,
      {{implicit_name_predicate, "svc-flow"},
       {{intersect, 
            {{implicit_name_predicate,"flow-step"},
            {recursive, {implicit_name_predicate, "step-step"}}}},
                {subquery,
                    {union,
                        {{implicit_name_predicate, "step-ifc"},
                            {union, 
                                {{intersect,
                                    {{implicit_name_predicate, "step-appcomp"},
                                        {intersect, 
                                            {{negated_traversal,
                                                {implicit_name_predicate, "platform-appcomp"}},
                                            {implicit_name_predicate, "plat-subPlat"}}}}},
             {implicit_name_predicate,"step-org"}}}}}}}}}.


