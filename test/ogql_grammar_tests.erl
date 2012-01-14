%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(ogql_grammar_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("semver/include/semver.hrl").

-compile(export_all).

-import(ogql, [parse/1, filter/2, consumer/1, provider/1,
               intersect/2, default/1, type/1, predicate/1, like/2, eq/2,
               gt/2, internal/1, semver/1, starts_with/2, value/1,
               contains/2, conjunction/2, recursive/1, union/2]).

basic_parsing_test() ->
    ?assertThat(typeof("platformSystem"), is(implicit_name_predicate)).

multiple_path_steps_test_() ->
    [?_assertThat(parts("platformSystem, systemConsumingPlatform"),
        is(equal_to([{implicit_name_predicate, "platformSystem"},
                     {implicit_name_predicate, "systemConsumingPlatform"}]))),
    ?_assertThat(parse("platformSystem => systemConsumingPlatform"),
        is(equal_to(intersect("platformSystem", "systemConsumingPlatform"))))].

filter_test_() ->
    [{"filtering an implicit name predicate",
      ?_assertThat(parse("service-interface[provider::name eq 'BPP']"),
                   is(equal_to(filter(predicate("service-interface"),
                                eq(provider("name"), "BPP")))))},
     {"filtering by member names containing unusual characters",
      ?_assertThat(parse("Person[::post-code starts_with 'SE9']"),
                  is(equal_to(filter(predicate("Person"),
                                starts_with(default("post-code"), "SE9")))))},
     {"filtering with pseudo-function as operator",
      ?_assertThat(parse("service-interface[consumer::processId starts_with '1349']"),
                  is(equal_to(filter(predicate("service-interface"),
                               starts_with(consumer("processId"), "1349")))))},
     {"filtering with a wilcarded type name predicate",
      ?_assertThat(parse("Person => ?[provider::name eq 'Joe']"),
            is(equal_to(
                    intersect(predicate("Person"),
                              filter(predicate("?"),
                                    eq(provider("name"), "Joe"))))))},
     {"filtering with white space between steps",
      ?_assertThat(parse("Department[::region eq 'Greater London'] =>
                            Employee[::firstName like 'John%']"),
            is(equal_to(
                    intersect(filter(predicate("Department"),
                                eq(default("region"), "Greater London")),
                              filter(predicate("Employee"),
                                like(default("firstName"), "John%"))))))}].

accessing_edge_nodes_test_() ->
    [{"access to 'special' object fields",
     ?_assertThat(parse("?[::$(name) eq 'Caller' AND
                            ::$(description) contains 'APMO']"),
         is(equal_to(
            filter(predicate("?"),
                conjunction(
                    eq(default(internal("name")), "Caller"),
                    contains(default(internal("description")), "APMO"))))))},
     {"access to object fields via fully qualified names",
     ?_assertThat(parse("?[::$(version.major) gt 10 AND
                           ::$(lastmodified.user) eq 991726352]"),
         is(equal_to(
            filter(predicate("?"),
                conjunction(
                    gt(default(internal({"version", "major"})), 10),
                    eq(default(
                        internal({"lastmodified", "user"})), 991726352))))))},
     {"access to fully qualified version numbers",
     ?_assertThat(parse("?[::$(version.major) eq 2 AND
                           ::$(version.minor) eq 0 AND
                           ::$(version.build) eq 1]"),
         is(equal_to(
            filter(predicate("?"),
                conjunction(
                    conjunction(
                        eq(default(internal({"version", "major"})), 2),
                        eq(default(internal({"version", "minor"})), 0)),
                    eq(default(internal({"version","build"})), 1))))))}].

version_handling_test() ->
     ?assertThat(parse("?[::$(version) gt VSN(1.6.13-RC2)]"),
            is(equal_to(filter(predicate("?"),
                        gt(default(internal("version")),
                            semver("1.6.13-RC2")))))).

recursive_join_operator_test_() ->
    [{"when applied to implicit name predicates",
     ?_assertThat(parse("ancestry-person,*person-person"),
            is(equal_to(union(predicate("ancestry-person"),
                              recursive(predicate("person-person"))))))},
     {"when applied to join groups",
       ?_assertThat(parse("platform-system =>
              *((system-system, system-interface) =>
                  interface-system)"),
            is(equal_to(
                intersect(predicate("platform-system"),
                          recursive(
                            intersect(
                                union(predicate("system-system"),
                                      predicate("system-interface")),
                                predicate("interface-system")))))))},
     {"when applied to traversal order groups",
       ?_assertThat(parse("*(personRoles => roleRelationship)"),
            is(equal_to(recursive(intersect(predicate("personRoles"),
                                            predicate("roleRelationship"))))))}].

literal_handling_test_() ->
    [{"separate handling of strings and integers",
     ?_assertThat(parse("Person[::name like 'Joe' AND ::age gt 18]"),
        is(equal_to(
            filter(predicate("Person"),
                   conjunction(like(default("name"), "Joe"),
                               gt(default("age"), 18))))))}].

literal_handling_test_x() ->
    [{"separate handling of strings and integers",
     ?_assertThat(parse("Person[::name like 'Joe' AND ::age > 18]"),
                  is(equal_to([{{type_name_predicate,"Person"},
                                {filter_expression,
                                 [{conjunction,
                                   [[{default_axis, {member_name,"name"}},
                                     {operator,"like"},
                                     {literal,"Joe"}],
                                    [{default_axis, {member_name,"age"}},
                                     {operator,">"},
                                     {literal, 18}]]}]}}])))},
     {"separate handling of strings and floats",
      ?_assertThat(parse("Person[::name like 'Joe' AND ::age > 21.65]"),
                   is(equal_to([{{type_name_predicate,"Person"},
                                 {filter_expression,
                                  [{conjunction,
                                    [[{default_axis, {member_name,"name"}},
                                      {operator,"like"},
                                      {literal,"Joe"}],
                                     [{default_axis, {member_name,"age"}},
                                      {operator,">"},
                                      {literal, 21.65}]]}]}}])))},
     {"date handling is done via a pseudo-function",
     ?_assertThat(parse("Person[::date-of-birth > DATE(21-3-1972)]"),
          contains_date_literal({1972,3,21}))},
     {"boolean handling via a built-in constants",
     ?_assertThat(parse("Service[::is-daemon = TRUE OR ::active = FALSE]"),
          is(equal_to([{{type_name_predicate, "Service"},
                           {filter_expression,
                            [{disjunction,
                              [[{default_axis, {member_name, "is-daemon"}},
                                {operator,"="},
                                {literal, {boolean,true}}],
                               [{default_axis, {member_name, "active"}},
                                {operator,"="},
                                {literal, {boolean, false}}]]}]}}])))},
     {"user defined constants",
     ?_assertThat(parse("Service[::classification = :strategic]"),
           is(equal_to([{{type_name_predicate,"Service"},
                            {filter_expression,
                             [{default_axis, {member_name, "classification"}},
                              {operator,"="},
                              {literal, {constant, strategic}}]}}])))}].

logical_operator_test_x() ->
    [{"single logical conjunction",
     ?_assertThat(parse("Person[::post-code starts_with 'SE9' AND ::name like 'Joe']"),
                  is(equal_to([{{type_name_predicate, "Person"},
                               {filter_expression, [
                                {conjunction,
                                  [[{default_axis, {member_name, "post-code"}},
                                    {operator, "starts_with"},
                                    {literal, "SE9"}],
                                   [{default_axis, {member_name, "name"}},
                                    {operator, "like"},
                                    {literal, "Joe"}]]}]}}])))},
     {"multiple logical conjunctions",
     ?_assertThat(parse("Person[::post-code starts_with 'SE9' AND "
                                "::name like 'Joe' AND "
                                "::contact_details contains 'Besborough']"),
                  is(equal_to([{{type_name_predicate, "Person"},
                                 {filter_expression, [
                                    {conjunction,
                                        [[{conjunction,
                                          [[{default_axis, {member_name, "post-code"}},
                                            {operator, "starts_with"},
                                            {literal, "SE9"}],
                                           [{default_axis, {member_name, "name"}},
                                            {operator, "like"},
                                            {literal, "Joe"}]]}],
                                     [{default_axis, {member_name, "contact_details"}},
                                      {operator, "contains"},
                                      {literal, "Besborough"}]]}]}}])))},
      {"mixed disjunctions and conjunctions",
      ?_assertThat(parse("Person[::post-code starts_with 'SE9' AND "
                                 "::name like 'Joe' OR "
                                 "consumer::contact_details contains 'Besborough']"),
                   is(equal_to([{{type_name_predicate, "Person"},
                             {filter_expression,
                                [{disjunction,
                                    [[{conjunction,
                                      [[{default_axis, {member_name, "post-code"}},
                                     {operator, "starts_with"},
                                     {literal, "SE9"}],
                                     [{default_axis, {member_name, "name"}},
                                     {operator, "like"},
                                     {literal, "Joe"}]]}],
                                    [{{axis, "consumer"},
                                        {member_name, "contact_details"}},
                                  {operator, "contains"},
                                  {literal, "Besborough"}]]}]}}])))}].

grouping_test_x() ->
    [{"fixed order grouping",
     ?_assertThat(parse("a-b,(b-c, b-d)"),
        is(equal_to([{implicit_name_predicate,"a-b"},
                        {fixed_order_group,[
                            {implicit_name_predicate,"b-c"},
                            {implicit_name_predicate,"b-d"}]}])))},
     {"traversal order grouping",
     ?_assertThat(parse("a-b,{b-c, c-d}"),
        is(equal_to([{implicit_name_predicate,"a-b"},
                        {traversal_order_group,[
                            {implicit_name_predicate,"b-c"},
                            {implicit_name_predicate,"c-d"}]}])))},
     {"nested grouping with filter predicates",
     ?_assertThat(parse("server-interface,
                            {interface-client[::group = :b2b],
                                client-sla,(sla-groups[::$(key) = :b2b],sla-documents)}"),
        is(equal_to([{implicit_name_predicate, "server-interface"},
                      {traversal_order_group,
                         [{{implicit_name_predicate, "interface-client"},
                           {filter_expression,
                            [{default_axis, {member_name, "group"}},
                             {operator, "="},
                             {literal, {constant, b2b}}]}},
                          {implicit_name_predicate, "client-sla"},
                          {fixed_order_group,
                            [{{implicit_name_predicate, "sla-groups"},
                             {filter_expression,
                              [{default_axis, {member_name, {internal, "key"}}},
                               {operator, "="},
                               {literal, {constant, b2b}}]}},
                            {implicit_name_predicate, "sla-documents"}]}]}])))}].

%recursive_join_conditions_test_() ->
%    [{}].

%%
%% Utility functions and custom Hamcrest matchers
%%

parts(Q) ->
    [ {type(P), value(P)} ||
                    P <- tuple_to_list(ogql:parts(parse(Q))) ].

typeof(Q) ->
    type(parse(Q)).

contains_date_literal(Date) ->
    fun (AST) ->
        case (AST) of
            [{{type_name_predicate,"Person"},
               {filter_expression,
                [{default_axis, {member_name, "date-of-birth"}},
                 {operator,">"},
                 {literal, {date, {Date, _}}}]}}] -> true;
            _ -> false
        end
    end.
