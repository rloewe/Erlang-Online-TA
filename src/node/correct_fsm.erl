-module(correct_fsm).
-behaviour (gen_fsm).
-export([start_link/1]).
-export([start_job/2]).
-export ([init/1,listen/2,correction/2,finished/2]).

start_job(Name,{Prog,Args}) ->
    gen_fsm:send_event(Name,{start_assign,{Prog,Args}}).

start_link({Name,Node}) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Node], []).

init([Node]) ->
    {ok, listen, Node}.

listen({start_assign,{Prog,Args,SessionToken}},Node) ->
    io:format("Assignment received~n"),
    {next_state,correction,{Prog,Args,Node,SessionToken},0}.

correction(timeout,{Prog,Args,Node}) ->
    %MAGIC
    io:format("Correction state ~n"),
    Res = os:cmd("ls"),
    {next_state,finished,{Res,Node},0}.

finished(timeout,{Res,Node}) ->
    %TODO, API for node for result
    io:format("Result received ~n~p~n",[Res]),
    {next_state,listen,Node}.



