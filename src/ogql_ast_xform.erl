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
    case Node of
        [[_, Word]] ->
            {implicit_name_predicate, bin_parts_to_string(Word)};
        _ ->
            Node
    end;
transform(_, Node, _) ->
    Node.

bin_parts_to_string(Parts) -> 
    lists:concat([ erlang:binary_to_list(B) || B <- Parts ]).

drop_sep(Node) ->
    H = proplists:get_value(head, Node),
    T = [R || [_,R] <- proplists:get_value(tail, Node)],
    [H|T].
