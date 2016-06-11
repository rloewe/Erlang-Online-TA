-module(correct_fsm).
-behaviour (gen_fsm).
-export([start_link/1]).
-export([start_job/4, job_done/2]).
-export ([init/1,listen/2,correction/2,finished/2,terminate/3,handle_event/3,code_change/4,handle_info/3]).
-import (node_server, [finish_handin_job/3]).

start_job(FsmPID,Module,FilePath,SessionToken) ->
    gen_fsm:send_event(FsmPID,{start_assign,{Module,FilePath,SessionToken}}).

job_done(FsmPID, Res) ->
    gen_fsm:send_event(FsmPID, Res).

start_link({Node}) ->
    gen_fsm:start_link(?MODULE, [Node], []).

init([Node]) ->
    {ok, listen, Node}.

listen({start_assign,{Type,FilePath,SessionToken}},Node) ->
    io:format("Handin received~n"),
    {next_state,correction,{Type,FilePath,Node,SessionToken},0}.

correction(timeout,{Type,FilePath,Node,SessionToken}) ->
    io:format("Correction state ~n"),
    gen_assignment:run(Type, FilePath, self()),
    {next_state,finished,{Node,SessionToken}}.

finished({ok, Res}, {Node, SessionToken}) ->
    finish_handin_job(Node, SessionToken, Res),
    io:format("Result received ~n~p~n",[Res]),
    {next_state, listen, Node};
finished({error, ErrorMsg},{Node,SessionToken}) ->
    finish_handin_job(Node,SessionToken,ErrorMsg),
    io:format("Error received: ~p~n",[ErrorMsg]),
    {next_state,listen,Node}.


terminate(_Reason, _StateName, _StateData) ->
    io:format("test"),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

handle_event(_Event, StateName, Data) ->
    {next_state, StateName, Data}.
handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.
