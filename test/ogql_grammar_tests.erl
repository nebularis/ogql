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

basic_parsing_test() ->
    ?assertThat(parsed("platformSystem"),
                is(equal_to([{implicit_name_predicate, "platformSystem"}]))).

multiple_path_steps_test() ->
    ?assertThat(parsed("platformSystem,systemConsumingPlatform"),
                is(equal_to(
                    [{implicit_name_predicate, "platformSystem"},
                     {implicit_name_predicate, "systemConsumingPlatform"}]))).

%implicit_name_and_type_filter_equivalence_test() ->
%    ?assertThat(parsed("Person"),
%                is(equal_to(parsed("?[provider::$type = 'Person']"))).

filter_test_() ->
    [{"filtering an implicit name predicate",
      ?_assertThat(parsed("service-interface[provider::name = 'BPP']"),
                   is(equal_to([{{implicit_name_predicate, "service-interface"},
                                {filter_expression, [
                                    {{axis,"provider"},{member_name,"name"}},
                                    {operator,"="},
                                    {literal,"BPP"}]}}])))},
     {"filtering by member names containing unusual characters",
      ?_assertThat(parsed("Person[::post-code starts_with 'SE9']"),
                  is(equal_to([{{type_name_predicate,"Person"},
                                 {filter_expression,
                                  [{default_axis,
                                    {member_name,"post-code"}},
                                   {operator,"starts_with"},
                                   {literal,"SE9"}]}}])))},
     {"filtering with pseudo-function as operator",
      ?_assertThat(parsed("service-interface[consumer::processId starts_with '1349']"),
                  is(equal_to([{{implicit_name_predicate, "service-interface"},
                               {filter_expression, [
                                    {{axis,"consumer"},{member_name,"processId"}},
                                    {operator,"starts_with"},
                                    {literal,"1349"}]}}])))},
     {"filtering with a wilcarded type name predicate",
      ?_assertThat(parsed("Person,?[provider::name = 'Joe']"),
                  is(equal_to([{type_name_predicate,"Person"},
                               {{type_name_predicate,"?"},
                               {filter_expression,
                                   [{{axis,"provider"},{member_name,"name"}},
                                    {operator,"="},
                                    {literal,"Joe"}]}}])))},
    {"filtering with a wilcarded type name predicate",
     ?_assertThat(parsed("Person,?[provider::name = 'Joe']"),
                 is(equal_to([{type_name_predicate,"Person"},
                              {{type_name_predicate,"?"},
                              {filter_expression,
                                  [{{axis,"provider"},{member_name,"name"}},
                                   {operator,"="},
                                   {literal,"Joe"}]}}])))},
    {"filtering with white space between steps",
     ?_assertThat(parsed("Department[::region = 'Greater London'],
                            Employee[::firstName like 'John%']"),
                 is(equal_to([{{type_name_predicate, "Department"},
                                {filter_expression,
                                 [{default_axis, {member_name, "region"}},
                                  {operator,"="},
                                  {literal, "Greater London"}]}},
                               {{type_name_predicate, "Employee"},
                                {filter_expression,
                                 [{default_axis, {member_name, "firstName"}},
                                  {operator,"like"},
                                  {literal, "John%"}]}}])))}].

accessing_edge_nodes_test_() ->
    [{"access to basic object fields",
     ?_assertThat(parsed("?[::$name = 'Caller' AND 
                            ::$description contains 'APMO']"),
                 is(equal_to([{{type_name_predicate, "?"},
                                {filter_expression,
                                  [{conjunction,
                                    [ [
                                     {default_axis, 
                                        {member_name, {internal, "name"}}},
                                        {operator, "="},
                                        {literal, "Caller"}],
                                     [{default_axis,
                                        {member_name, {internal, "description"}}},
                                        {operator, "contains"},
                                        {literal, "APMO"}]]}]}}])))}].

literal_handling_test_() ->
    [{"separate handling of strings and integers",
     ?_assertThat(parsed("Person[::name like 'Joe' AND ::age > 18]"),
                  is(equal_to([{{type_name_predicate,"Person"},
                                {filter_expression,
                                 [{conjunction,
                                   [[{default_axis, {member_name,"name"}},
                                     {operator,"like"},
                                     {literal,"Joe"}],
                                    [{default_axis, {member_name,"age"}},
                                     {operator,">"},
                                     {literal, {integer, 18}}]]}]}}])))},
     {"separate handling of strings and floats",
      ?_assertThat(parsed("Person[::name like 'Joe' AND ::age > 21.65]"),
                   is(equal_to([{{type_name_predicate,"Person"},
                                 {filter_expression,
                                  [{conjunction,
                                    [[{default_axis, {member_name,"name"}},
                                      {operator,"like"},
                                      {literal,"Joe"}],
                                     [{default_axis, {member_name,"age"}},
                                      {operator,">"},
                                      {literal, {float, 21.65}}]]}]}}])))},
     {"date handling is done via a pseudo-function",
     ?_assertThat(parsed("Person[::date-of-birth > DATE(21-3-1972)]"),
          contains_date_literal({1972,3,21}))}].

contains_date_literal(Date) ->
    fun (AST) -> 
        case (AST) of
            [{{type_name_predicate,"Person"},
               {filter_expression,
                [{default_axis,
                  {member_name,
                   "date-of-birth"}},
                 {operator,">"},
                 {literal,
                  {date,
                   {Date, _}}}]}}] -> true;
            _ -> false
        end
    end.

logical_operator_test_() ->
    [{"single logical conjunction",
     ?_assertThat(parsed("Person[::post-code starts_with 'SE9' AND ::name like 'Joe']"),
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
     ?_assertThat(parsed("Person[::post-code starts_with 'SE9' AND "
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
      ?_assertThat(parsed("Person[::post-code starts_with 'SE9' AND "
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

parsed(Q) ->
    ogql_grammar:parse(Q).

