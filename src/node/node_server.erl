-module(node_server).
-behaviour(gen_server).
-import (master_server, [connect_to/3]).
-import (config_parser, [parse/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, queue_handin_job/4,finish_handin_job/3,add_assignment/4,add_module/3]).


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
    gen_server:call({?MODULE, Node}, {queue_job, AssignmentID, Files, SessionToken}).

%API call for finishing an assignment on a node server, should be called from the 
%FSM assosiated to the job. Returns ok
finish_handin_job(Node,SessionToken,Res) ->
    gen_server:call({?MODULE,Node},{finish_job,{SessionToken,Res}}).

%API call for adding an assignment to the node server
add_assignment(Node,AssignmentID,AssignmentDict,Files) ->
    gen_server:call({?MODULE,Node},{add_assignment,AssignmentID,AssignmentDict,Files}).

%Save module binary on node server, takes the modulename and module binary as arguments
add_module(Node,ModuleName,ModuleBinary) -> 
    gen_server:call({?MODULE,Node},{add_module,ModuleName,ModuleBinary}).


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
  {queue_job, AssignmentID, Files, SessionToken},
  _From,
  {Queue, Assignments, CurrentJobs, MasterNode}
 ) ->
    %TODO handle assignment id
    %TODO fix magic constant
    %TODO move file handling to seperate thread
    %TODO Add subfolders for each handin
    Path = "./handins/",
    save_files(Files,Path),
    Size = dict:size(CurrentJobs),
    if  
        Size < 2 ->
            case start_handin(AssignmentID,Assignments,Path,SessionToken) of
                {ok,FsmPID} ->
                    NewCurrentJobs = dict:store(SessionToken,FsmPID,CurrentJobs),
                    io:format("Queue: ~p ~nCurrentJobs: ~p", [Queue, NewCurrentJobs]),
                    {reply, started, {Queue, Assignments, NewCurrentJobs, MasterNode}};
                {error,noassign} ->
                    %TODO handle getting assignment
                    {reply,notstarted,{Queue,Assignments,CurrentJobs,MasterNode}}
                    end;
        true ->
            NewQueue = queue:in({AssignmentID,Path,SessionToken},Queue),
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
  {add_assignment,AssignmentID,AssignmentDict,Files}, _From, 
  {Queue, Assignments, CurrentJobs, MasterNode}) ->
    %TODO add some functionality
    Path = "./AssignmentFiles/"++AssignmentID ++ "/",
    case file:make_dir(Path) of
      Pat when Pat =:= ok; Pat =:= {error,eexist} ->
          save_files(Files,Path),
          NewAssignments = dict:store(AssignmentID,AssignmentDict,Assignments),
          {reply, ok, {Queue,NewAssignments,CurrentJobs,MasterNode}};
      E -> 
        {reply, {error,E},{Queue,Assignments,CurrentJobs,MasterNode}}
    end;

handle_call({add_module,ModuleName,ModuleBinary}, _From, State) ->
    Path = "./Modules/" ++ atom_to_list(ModuleName),                    
    case file:write_file(Path++ ".beam",ModuleBinary) of
        ok ->
            case code:load_abs(Path) of 
                {module,Module} ->
                    {reply,ok,State};
                {error,Reason} ->
                    {reply,{error,Reason},State}
            end;                    
        {error,Reason} ->
            {reply, {error,Reason}, State}  
    end;    

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.



save_files([],_) ->
  ok;
save_files([{FileName,File} | Rest ],Path) ->
  file:write_file(Path++FileName,File),
  save_files(Rest,Path).

start_handin(AssignmentID,Assignments,FilePath,SessionToken) ->
    case dict:find(AssignmentID,Assignments) of 
        {ok,AssignDict} ->
            FsmPID = correct_fsm:start_link({node()}),
            correct_fsm:start_job(FsmPID,AssignDict,FilePath,SessionToken),
            {ok,FsmPID};
        error ->
            %TODO Handle some call back to request from master
            {error,noassign}
    end.
