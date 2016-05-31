-module(node_server).
-behaviour(gen_server).
-import (master_server, [connect_to/3]).
-import (config_parser, [parse/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, queue_handin_job/4,finish_handin_job/3,add_assignment/2]).


% API call to start the node server, takes a path to a node server config file as argument
% Returns {ok,PID} or {error,Message}. 
start(Path) ->
    %TODO add parse config
    case parse(Path, false) of
        {ok, Config} ->
            %exec:start([]),
            Cookie = dict:fetch("Cookie", Config),
            MasterNode = dict:fetch("Master", Config),
            Specs = none,
            erlang:set_cookie(node(),Cookie),
            net_kernel:connect_node(MasterNode),
            gen_server:start_link({local, ?MODULE}, ?MODULE, [MasterNode,Specs], []);
        {error, Msg} ->
            io:format("~p", [Msg]),
            {error,Msg}
    end.

%API call for queueing a new job on a node server, should be called from the 
%master server. Returns started or queued depending on if the job is started.
queue_handin_job(Node, AssignmentID, Files, SessionToken) ->
    gen_server:call({?MODULE, Node}, {queue_job, {AssignmentID, Files, SessionToken}}).

%API call for finishing an assignment on a node server, should be called from the 
%FSM assosiated to the job. Returns ok
finish_handin_job(Node,SessionToken,Res) ->
    gen_server:call({?MODULE,Node},{finish_job,{SessionToken,Res}}).

%API call for adding an assignment to the node server
add_assignment(Node,{AssignmentID,Module}) ->
    gen_server:call({?MODULE,Node},{add_assignment,AssignmentID,Module}).


init([MasterNode,Specs]) ->
    %master:connect_to(node()),
    case connect_to(node(),Specs,MasterNode) of
        ok ->
            Queue = queue:new(),
            Assignments = dict:new(),
            CurrentJobs = dict:new(),
            %Supervisor = supervisor:
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
    Size = dict:size(CurrentJobs),
    if Size < 2 ->
           {AssignmentID, Files, SessionToken} = Assignment,
           FsmPID = correct_fsm:start_link({node()}),
           correct_fsm:start_job(FsmPID,{none,none,SessionToken}),
           NewCurrentJobs = dict:store(SessionToken,{AssignmentID,Files,FsmPID},CurrentJobs),
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
    master_server:update_handin_job(SessionToken,{finished,Res},MasterNode),
    {reply, ok, {Queue, Assignments, NewCurrentJobs, MasterNode}};    

handle_call(
  {add_assignment,AssignmentID,Module}, _From, 
  {Queue, Assignments, CurrentJobs, MasterNode}) ->
    %TODO add some functionality
    Module:hello(),
    NewAssignments = dict:store(AssignmentID,Module,Assignments),
    {reply, ok, {Queue,NewAssignments,CurrentJobs,MasterNode}};


handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
