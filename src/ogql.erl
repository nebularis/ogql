%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2012 Tim Watson (watson.timothy@gmail.com)
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
-module(ogql).
-include_lib("annotations/include/annotations.hrl").
-include_lib("semver/include/semver.hrl").
-compile(export_all).

parse(Q) ->
    case ogql_grammar:parse(Q) of
        {_, Bin, {{line,_},{column,_}}=Pos} when is_binary(Bin) ->
            throw({parse_error, Pos, Bin});
        AST ->
            AST
    end.

semver(#semver{}=Vsn) ->
    Vsn;
semver(Vsn) when is_list(Vsn) ->
    semver:parse(Vsn).

%% cursor operations

left({Left, {operator, _}, _}) ->
    Left;
left({Left, _}) ->
    Left.

right({{_Axis, _}, {operator, _}, Right}) ->
    Right;
right({_, Right}) ->
    Right.

parts({_, {_, _}=Parts}) ->
    Parts.

type({T, _}) ->
    T.

value({_, V}) ->
    V.

has_filter({_, {filter_expression, _}}) ->
    true;
has_filter(_) ->
    false.

%% AST builders

constant(What) ->
    {constant, What}.

recursive(Thing) ->
    {recursive, Thing}.

internal(Tag) when is_list(Tag) orelse is_tuple(Tag) ->
    {internal, Tag}.

default(Name) ->
    axis(default, Name).

provider(Name) ->
    axis(provider, Name).

consumer(Name) ->
    axis(consumer, Name).

axis(Axis, {internal, _}=Name) ->
    {axis(Axis), {member_name, Name}};
axis(Axis, [H|_]=Name) when is_integer(H) ->
    {axis(Axis), {member_name, Name}}.

axis(default) ->
    default_axis;
axis(AxisTag) when is_atom(AxisTag) ->
    {axis, AxisTag}.

predicate(Id) ->
    Parsed = parse(Id),
    case Parsed of
        {T, _} when T =:= implicit_name_predicate orelse
                    T =:= type_name_predicate ->
            Parsed;
        Other ->
            throw({invalid_predicate, Other})
    end.

filter(What, Filter) ->
    {What, {filter_expression, maybe_parse_filter_expression(Filter)}}.

conjunction(A, B) when is_tuple(A) andalso is_tuple(B) ->
    {conjunction, {A, B}}.

disjunction(A, B) when is_tuple(A) andalso is_tuple(B) ->
    {disjunction, {A, B}}.

union(A, B) when is_tuple(A) andalso is_tuple(B) ->
    {union, {A, B}};
union([A1|_]=A, [B1|_]=B) when
    is_integer(A1) andalso
    is_integer(B1) ->
    union(parse(A), parse(B)).

intersect(A, B) when is_tuple(A) andalso is_tuple(B) ->
    {intersect, {A, B}};
intersect([A1|_]=A, [B1|_]=B) when
    is_integer(A1) andalso
    is_integer(B1) ->
    intersect(parse(A), parse(B)).

maybe_parse_filter_expression([H|_]=Filter) when is_integer(H) andalso
                                                 is_list(Filter) ->
    parse(Filter);
maybe_parse_filter_expression(Filter) ->
    Filter.

-delegate([{args, ['$T', '$I']},
           {arity, 2},
           {delegate, [
                "eq", "gt",
                "gteq", "lt",
                "lteq", "like", 
                "contains", "starts_with",
                "ends_with", "matches", "path_exists"
            ]}]).
binop(Op, Axis, {{_,_,_}, _}=Literal) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, Literal) when Literal =:= true orelse Literal =:= false ->
    binop(Op, Axis, {literal, {boolean, Literal}});
binop(Op, Axis, Literal) when is_integer(Literal) orelse
                              is_float(Literal) orelse
                              is_list(Literal) orelse
                              is_record(Literal, semver) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, {constant, _}=Literal) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, {literal, _}=Literal) ->
    {Axis, {operator, Op}, Literal}.

