-module(node_server).
-behaviour(gen_server).
-import (master_server, [connect_to/3]).
-import (config_parser, [parse/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, queue_assignment_job/4,finish_assignment_job/3]).

start(Path) ->
    %TODO add parse config
    case parse(Path, false) of
        {ok, Config} ->
            Cookie = dict:fetch("Cookie", Config),
            MasterNode = dict:fetch("Master", Config),
            Specs = none,
            erlang:set_cookie(node(),Cookie),
            net_kernel:connect_node(MasterNode),
            gen_server:start_link({local, ?MODULE}, ?MODULE, [MasterNode,Specs], []);
        {error, Msg} ->
            io:format("~p", [Msg])
    end.

queue_assignment_job(Node, AssignmentID, Files, SessionToken) ->
    gen_server:call({?MODULE, Node}, {queue_job, {AssignmentID, Files, SessionToken}}).

finish_assignment_job(Node,SessionToken,Res) ->
    gen_server:call({?MODULE,Node},{finish_job,{SessionToken,Res}}).

init([MasterNode,Specs]) ->
    %master:connect_to(node()),
    case connect_to(node(),Specs,MasterNode) of
        ok ->
            Queue = queue:new(),
            Assignments = dict:new(),
            CurrentJobs = dict:new(),
            {ok, {Queue,Assignments,CurrentJobs,MasterNode}};
        A ->
            io:format("~p",[A]),
            {stop,nocon}
    end.


handle_cast(_Message, State) ->
    {noreply, State}.


handle_call(
  {queue_job, Assignment},
  _From,
  {Queue, Assignments, CurrentJobs, MasterNode}
 ) ->
    %TODO handle assignment id
    %TODO fix magic constant
    %TODO Name FSM uniquely
    %TODO Dont have a hardcoded fsm name.....
    if length(CurrentJobs) < 2 ->
           {AssignmentID, Files, SessionToken} = Assignment,
           Fsm = correct_fsm:start_link({fsm1,node()}),
           correct_fsm:start_job(fsm1,{none,none,SessionToken}),
           NewCurrentJobs = dict:store(SessionToken,{AssignmentID,Files},CurrentJobs),
           io:format("Queue: ~p ~nCurrentJobs: ~p", [Queue, NewCurrentJobs]),
           {reply, started, {Queue, Assignments, NewCurrentJobs, MasterNode}};
       true ->
           NewQueue = queue:in(Assignment, Queue),
           io:format("Queue: ~p ~nCurrentJobs: ~p", [NewQueue, CurrentJobs]),
           {reply, queued, {NewQueue, Assignments, CurrentJobs, MasterNode}}
    end;


handle_call(
  {finish_job,{SessionToken,Res}},
  _From,
  {Queue, Assignments, CurrentJobs, MasterNode}) ->
    %TODO add jobs from queue to running
    %TODO Handle errorhandling with master communication?
    %TODO Kill FSM
    NewCurrentJobs = dict:erase(SessionToken,CurrentJobs),
    master_server:assignment_job_updated(SessionToken,{finished,Res},MasterNode),
    {reply, ok, {Queue, Assignments, NewCurrentJobs, MasterNode}};    

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
