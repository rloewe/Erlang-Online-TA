-module(correct_fsm).   
-behaviour (gen_fsm).
-export([start_link/1]).
-export([start_job/2]).
-export ([init/1,listen/2,correction/2,finished/2,terminate/3,handle_event/3]).
-import (node_server, [finish_assignment_job/3]).
start_job(PID,{Prog,Args,SessionToken}) ->
    gen_fsm:send_event(PID,{start_assign,{Prog,Args,SessionToken}}).

start_link({Node}) ->
    gen_fsm:start_link(?MODULE, [Node], []).

init([Node]) ->
    {ok, listen, Node}.

listen({start_assign,{Prog,Args,SessionToken}},Node) ->
    io:format("Assignment received~n"),
    {next_state,correction,{Prog,Args,Node,SessionToken},0}.

correction(timeout,{Prog,Args,Node,SessionToken}) ->
    %MAGIC
    io:format("Correction state ~n"),
    Res = os:cmd("ls"),
    {next_state,finished,{Res,Node,SessionToken},0}.

finished(timeout,{Res,Node,SessionToken}) ->
    %TODO, API for node for result
    %TODO better errorhandling
    try 
        finish_assignment_job(Node,SessionToken,Res), 
        io:format("Result received ~n~p~n",[Res])
    catch
        _:_ -> io:format("test~n")
    end,
    {next_state,listen,Node}.


terminate(Reason, StateName, StateData) ->
    io:format("test"),
    ok.

code_change(OldVsn, StateName, StateData, Extra) -> {ok, StateName, StateData}.

handle_event(Event, StateName, Data) ->
    {next_state, StateName, Data}.