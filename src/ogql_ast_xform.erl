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
-module(ogql_ast_xform).
-export([transform/3]).

-type ast_node() :: list(term()) | tuple(atom(), ast_node()).

-spec transform(atom(), ast_node(), any()) -> ast_node().
transform(group, [_,[[_,Query]],_], _) ->
    Query;
transform(group, [_,[[_,[Query]]],_], _) ->
    Query;
transform('query', Node, _) ->
    case Node of
      [] -> [];
      [""] -> [];
      [[AST],[]] ->
            AST;
      [[Set], {subquery, _}=SubQuery] ->
            {Set, SubQuery};
        _ ->
            Node
    end;
transform(intersection, [A, _, B], _) ->
    Lv = case A of [L] -> L; _ -> A end,
    Rv = case B of [R] -> R; _ -> B end,
    {intersect, {Lv, Rv}};
transform(union, [A,_,B], _) ->
    {union, {A, B}};
transform(subquery, [_, Node], _) ->
    {subquery, Node};
transform(set, [[]|[Node]], _) ->
    Node;
transform(set, [{negated_traversal_operator,_}|[Data]], _) ->
    {negated_traversal, Data};
transform(set, [{negated_traversal_operator,_}|Data], _) ->
    {negated_traversal, Data};
transform(set, [{recursion_operator,_}|[Data]], _) ->
    {recursive, Data};
transform(set, [{recursion_operator,_}|Data], _) ->
    {recursive, Data};
transform(expression_list, Node, _) ->
    case Node of
        [] -> [];
        [""] -> [];
        _ ->
            H = proplists:get_value(head, Node),
            T = [ {J, P} || [J, P] <- proplists:get_value(tail, Node)],
            [H|T]
    end;
transform(name_predicate, Node, _) ->
    predicate(implicit_name_predicate, Node);
transform(type_predicate, Node, _) ->
    predicate(type_name_predicate, Node);
transform(axis_predicate, Node, _) ->
    predicate(axis_predicate, Node);
transform(anything_predicate, Node, _) ->
    predicate(any_type_predicate, Node);
transform(filter_predicate, Node, _) ->
    predicate(filter_predicate, Node);
transform(member_name, [<<"$(">>, Data, <<")">>], _) ->
    case string:tokens(bin_parts_to_string(Data), ".") of
        [Token] ->
            {member_name, {internal, Token}};
        Tokens when is_list(Tokens) ->
            {member_name, {internal, list_to_tuple(Tokens)}}
    end;
transform(data_point, [[], Member], _) ->
    Member;
transform(root_branch_filter, [_,[[_,{root_identifier, Node}]],_], _) ->
    case Node of
        [{primary_asset_id, [AssetId]}, [<<"-">>, Version]] ->
            {root_branch_filter, [AssetId, {version, Version}]};
        [{primary_asset_id, [AssetId]}, []] ->
            {root_branch_filter, [AssetId]}
    end;
transform(semver, Node, _) ->
    semver:parse(bin_parts_to_string(Node));
transform(asset_name, Node, _) ->
    {asset_name, bin_parts_to_string(Node)};
transform(operator, Op, _) ->
    {operator, list_to_atom(binary_to_list(Op))};
transform(data_point, [Axis, _, Member], _) ->
    case Axis of
        [] ->
            {default_axis, Member};
        {axis, Name} when is_list(Name) ->
            {{axis, list_to_atom(Name)}, Member};
        _ ->
            {Axis, Member}
    end;
transform(literal, {date, [_, Data, _]}, _) ->
    DateString = bin_parts_to_string(Data),
    {literal, {date, dh_date:parse(DateString)}};
transform(literal, {version, [_, Data, _]}, _) ->
    VsnString = bin_parts_to_string(Data),
    {literal, semver:parse(VsnString)};
transform(literal, {literal_int, Data}, _) ->
    {literal, list_to_integer(bin_parts_to_string(Data))};
transform(literal, {literal_float, Data}, _) ->
    {literal, list_to_float(bin_parts_to_string(Data))};
transform(literal, {literal_string, Node}, _) ->
    [_, Data, _] = Node,
    {literal, bin_parts_to_string(Data)};
transform(constant, [_,Data], _) ->
    {constant, list_to_atom(bin_parts_to_string(Data))};
transform(boolean, Node, _) ->
    {boolean, list_to_atom(string:to_lower(bin_parts_to_string(Node)))};
transform(NonTerminal, Node, _) ->
    case is_identifier(NonTerminal) of
        true ->
            Node;
        false ->
            Node2 = case Node of
                [[_,[H|_]=Word]] when is_list(Word) andalso is_binary(H) ->
                    bin_parts_to_string(Word);
                [[Node,[]]] ->
                    Node;
                Bin when is_binary(Bin) ->
                    erlang:binary_to_list(Bin);
                _ ->
                    Node
            end,
            {NonTerminal, Node2}
    end.

is_identifier(NonTerminal) ->
    lists:member(NonTerminal,
        [word, space, crlf, literal_number, normative_axis,
            data_point_or_literal, set, group, subquery]).

predicate(Type, Node) ->
    case Node of
        [[[[_, Word]]],[]] ->
            {Type, bin_parts_to_string(Word)};
        [[_, Word|_]|_] ->
            {Type, bin_parts_to_string(Word)};
        [[[_, Word]],[]] ->
            {Type, bin_parts_to_string(Word)};
        [[Word], {bracketed_expression, [_, [[_, ExpressionList]], _]}] ->
            {{Type, bin_parts_to_string(Word)},
                {filter_expression, reduce(ExpressionList)}};
        [[[_, Word]],  {bracketed_expression, [_, [[_, ExpressionList]], _]}] ->
                {{Type, bin_parts_to_string(Word)},
                 {filter_expression, reduce(ExpressionList)}};
        [[<<"?">>],[]] ->   
            %% NB: this clause occurs when we parse "?" by 
            %% itself from ogql:predicate/1
            {Type, "?"};
        [AST] ->
            {Type, AST};
        _ ->
            {Type, Node}
    end.

reduce(ExprList) ->
    lists:foldl(fun combine_expressions/2, [], ExprList).

combine_expressions({expression, Expr}, []) ->
    strip(Expr);
combine_expressions({{junction, {Type, _}}, {expression, Expr}}, Acc) ->
    {Type, {Acc, strip(Expr)}}.

strip(Expr) ->
    list_to_tuple([ X || X <- Expr, X /= <<" ">> andalso X /= [] ]).

bin_parts_to_string(Parts) when is_binary(Parts) ->
    erlang:binary_to_list(Parts);
bin_parts_to_string(Parts) ->
    lists:concat([ erlang:binary_to_list(B) || B <- lists:flatten(Parts) ]).

%drop_sep(Node) ->
%    H = proplists:get_value(head, Node),
%    T = [R || [_,R] <- proplists:get_value(tail, Node)],
%    [H|T].
