-module(safehaskell).

-behaviour(gen_assignment).

-export([setup/2, teardown/2, run/3]).

setup(_Config, _WorkingDir) ->
    done.

teardown(_Config, _WorkingDir) ->
    done.

run(Config, AssignmentDir, WorkingDir) ->
    {ok, RunScripts} = dict:find("runorder", Config),
    {
     ok,
     "cp -r " ++ WorkingDir ++ " " ++ AssignmentDir ++ "cd " ++ AssignmentDir ++
     "; ghc -XSafe " ++ string:join(lists:map(fun ({_, Elm}) -> Elm end, RunScripts), "; ghc -XSafe ") ++ "; " ++
     string:join(lists:map(fun ({_, Elm}) -> "./" ++ Elm end, RunScripts), "; "),
     0
    }.
