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

transform('query', Node, _) ->
    case Node of
      [] -> [];
      [""] -> [];
      _ -> drop_sep(Node)
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
transform(data_point, Node, _) ->
    [Axis, _, Member] = Node,
    {Axis, Member};
transform(literal, Node, _) ->
    [_, Data, _] = Node,
    {literal, bin_parts_to_string(Data)};
transform(NonTerminal, Node, _) ->
    case is_identifier(NonTerminal) of
        true ->
            Node;
        false ->
            Node2 = case Node of
                [[_,[H|_]=Word]] when is_list(Word) andalso is_binary(H) -> 
                    bin_parts_to_string(Word);
                Bin when is_binary(Bin) ->
                    erlang:binary_to_list(Bin);
                _ ->
                    Node
            end,
            {NonTerminal, Node2}
    end.

is_identifier(NonTerminal) ->
    lists:member(NonTerminal, 
        [word, space, crlf, sep, normative_axis, data_point_or_literal]).

predicate(Type, Node) ->
    case Node of
        [[[[_, Word]]],[]] ->
            {Type, bin_parts_to_string(Word)};
        [[_, Word|_]|_] ->
            {Type, bin_parts_to_string(Word)};
        [[[_, Word]],[]] ->
            {Type, bin_parts_to_string(Word)};
        [[[_, Word]],
            {bracketed_expression, [[_,
                [[_, {expression, Expr}]], _]]}] ->
            {{Type, bin_parts_to_string(Word)}, {filter_expression, Expr}};
        [[Word],
            {bracketed_expression, [[_,
                [[_, {expression, Expr}]], _]]}] ->
            {{Type, bin_parts_to_string(Word)}, {filter_expression, Expr}};
        _ ->
            {Type, Node}
    end.

bin_parts_to_string(Parts) when is_binary(Parts) ->
    erlang:binary_to_list(Parts);
bin_parts_to_string(Parts) ->
    lists:concat([ erlang:binary_to_list(B) || B <- lists:flatten(Parts) ]).

drop_sep(Node) ->
    H = proplists:get_value(head, Node),
    T = [R || [_,R] <- proplists:get_value(tail, Node)],
    [H|T].
