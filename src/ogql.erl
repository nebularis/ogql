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
-include_lib("semver/include/semver.hrl").
-compile(export_all).

parse(Q) ->
    ogql_grammar:parse(Q).

semver(#semver{}=Vsn) ->
    Vsn;
semver(Vsn) when is_list(Vsn) ->
    semver:parse(Vsn).

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

starts_with(Axis, Value) ->
    binop(starts_with, Axis, Value).

contains(Axis, Value) ->
    binop(contains, Axis, Value).

like(Axis, Value) ->
    binop(like, Axis, Value).

eq(Axis, Value) ->
    binop(eq, Axis, Value).

gt(Axis, Value) ->
    binop(gt, Axis, Value).

binop(Op, Axis, {{_,_,_}, _}=Literal) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, Literal) when is_integer(Literal) orelse
                              is_float(Literal) orelse
                              is_list(Literal) orelse
                              is_record(Literal, semver) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, {literal, _}=Literal) ->
    {Axis, {operator, Op}, Literal}.

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

parts({_, {_, _}=Parts}) ->
    Parts.

type({T, _}) ->
    T.

value({_, V}) ->
    V.

maybe_parse_filter_expression([H|_]=Filter) when is_integer(H) andalso 
                                                 is_list(Filter) ->
    parse(Filter);
maybe_parse_filter_expression(Filter) ->
    Filter.
