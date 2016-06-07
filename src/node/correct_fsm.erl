-module(correct_fsm).   
-behaviour (gen_fsm).
-export([start_link/1]).
-export([start_job/4]).
-export ([init/1,listen/2,correction/2,finished/2,terminate/3,handle_event/3]).
-import (node_server, [finish_assignment_job/3]).

start_job(FsmPID,Assignment,FilePath,SessionToken) ->
    gen_fsm:send_event(FsmPID,{start_assign,{Assignment,FilePath,SessionToken}}).

start_link({Node}) ->
    gen_fsm:start_link(?MODULE, [Node], []).

init([Node]) ->
    {ok, listen, Node}.

listen({start_assign,{Assignment,FilePath,SessionToken}},Node) ->
    io:format("Assignment received~n"),
    AssignmentID = dict:fetch("assignmentid",Assignment),
    Module = dict:fetch("module"),
    Runorder = dict:fetch("runorder"),
    {next_state,correction,{AssignmentID,Module,Runorder,FilePath,Node,SessionToken},0}.

%setup(timeout,{Prog,Args,Node,SessionToken}) ->


correction(timeout,{AssignmentID,Module,Runorder,FilePath,Node,SessionToken}) ->
    %MAGIC
    io:format("Correction state ~n"),
    Res = Module:run(AssignmentID,Runorder,FilePath),
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


terminate(_Reason, _StateName, _StateData) ->
    io:format("test"),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

handle_event(_Event, StateName, Data) ->
    {next_state, StateName, Data}.