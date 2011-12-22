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
-module(build_plugin).
-export([pre_compile/2, clean/2, pre_eunit/2]).

pre_compile(_, _) ->
    rebar_log:log(debug, "plugin working in ~s~n", [rebar_utils:get_cwd()]),
    case rebar_plugin_manager:is_base_dir() of
        true ->
            file:make_dir("generated");
        false ->
            rebar_log:log(debug, "Not base_dir~n", []),
            ok
    end,
    ok.

clean(_, _) ->
    case rebar_plugin_manager:is_base_dir() of
        true ->
            rebar_file_utils:rm_rf("generated");
        false ->
            ok
    end.

pre_eunit(_, _) ->
    case rebar_plugin_manager:is_base_dir() of
        true ->
            Grammar = "ogql_grammar.erl",
            Source = filename:join("generated", Grammar),
            Target = filename:join(".test", Grammar),
            rebar_utils:ensure_dir(Target),
            file:copy(Source, Target),
            true = filelib:is_regular(Target),
            ok;
        false ->
            rebar_log:log(debug, "Not base_dir~n", [])
    end.
