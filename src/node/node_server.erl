-module(node_server).
-behaviour(gen_server).
-import (master_server, [connect_to/2]).
-import (config_parser, [parse/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, queue_handin_job/5,finish_handin_job/3,add_assignment/4,add_module/3]).


% API call to start the node server, takes a path to a node server config file as argument,
% Creates the requires directories for files
% Returns {ok,PID} or {error,Message}.
start(Path) ->
    %TODO add parse config
    case parse(Path, false) of
        {ok, Config} ->
            %exec:start([]),
            Cookie = dict:fetch("Cookie", Config),
            MasterNode = dict:fetch("Master", Config),
            erlang:set_cookie(node(),Cookie),
            net_kernel:connect_node(MasterNode),
            gen_server:start_link({local, ?MODULE}, ?MODULE, [MasterNode], []);
        {error, Msg} ->
            io:format("~p", [Msg]),
            {error,Msg}
    end.

%API call for queueing a new job on a node server, should be called from the
%master server. Returns started or queued depending on if the job is started.
queue_handin_job(Node, AssignmentID, DirID, Files, SessionToken) ->
    gen_server:call({?MODULE, Node}, {queue_job, AssignmentID, DirID, Files, SessionToken}).

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

-record(nodeState, {queue, assignments, currentJobs, masterNode, modules, maxJobs}).


init([MasterNode]) ->
    %master:connect_to(node()),
    case connect_to(node(),MasterNode) of
        ok ->
            Queue = queue:new(),
            MaxJobs = 4,
            Assignments = dict:new(),
            %Currentjobs is a tuplelist of {FSMPid,FSMStatus,SessionToken, HandinArgs}, where HandinArgs is a tuple of remaining args to the given handin
            case helper_functions:create_dirs(["./Modules","./Handins","Assignments"]) of
                ok ->
                    {ok, #nodeState{
                            queue = Queue,
                            assignments = Assignments,
                            currentJobs = init_handin_fsms(MaxJobs), %CurrentJobs,
                            masterNode = MasterNode,
                            modules = dict:new(),
                            maxJobs = 4
                           }
                    };
                {error,E} ->
                    {stop,{error,E}}
            end;
        {error,nocon} ->
            io:format("Cannot connect to ~p \n",[MasterNode]),
            {stop,nocon}
    end.


handle_cast(
  {update_handin_status,Status,SessionToken},
  State) ->
    MasterNode = State#nodeState.masterNode,
    case Status of
        running ->
            master_server:update_handin_job(SessionToken,running,MasterNode),
            {noreply,State};
        queued ->
            master_server:update_handin_job(SessionToken,queued,MasterNode),
            {noreply,State}
    end;



handle_cast(
  {dequeue_job},State) ->
    CurrentJobs = State#nodeState.currentJobs,
    case queue:out(State#nodeState.queue) of
        {{value,{AssignmentID,DirID,SessionToken}},NewQueue} ->
            case dict:find(AssignmentID,State#nodeState.assignments) of
                {ok,{Pid,AssignDict}} ->
                    case lists:keysearch(free,2,CurrentJobs) of
                        %A FSM is not handling a job
                        {value,{FsmPID,_,_,_}} ->
                            NewCurrentJobs = lists:keyreplace(FsmPID,1,CurrentJobs,{FsmPID,inuse,SessionToken,{Pid,DirID}}),
                            spawn(fun() -> start_handin(FsmPID,Pid,DirID,SessionToken) end),
                            {noreply,State#nodeState{currentJobs = NewCurrentJobs,queue = NewQueue}};
                        false ->
                        %All FSM are inuse do nothing
                            {noreply,State}
                    end;
                error ->
                    {noreply,State}
            end;
        {empty,_} ->
            {noreply,State}
    end;

handle_cast(_Message, State) ->
    {noreply, State}.


handle_call(
  {queue_job, AssignmentID, DirID, Files, SessionToken},
  _From,
  State
 ) ->
    CurrentJobs = State#nodeState.currentJobs,
    case dict:find(AssignmentID,State#nodeState.assignments) of
        {ok,{Pid,AssignDict}} ->
            case lists:keysearch(free,2,CurrentJobs) of
                %A FSM is not handling a job
                {value,{FsmPID,_,_,_}} ->
                    NewCurrentJobs = lists:keyreplace(FsmPID,1,CurrentJobs,{FsmPID,inuse,SessionToken,{Pid,DirID}}),
                    spawn(fun() ->
                        file:make_dir("./Handins/" ++ DirID),
                        helper_functions:save_files(Files,"./Handins/" ++ DirID ++ "/"),
                        start_handin(FsmPID,Pid,DirID,SessionToken) end),
                    {reply,received,State#nodeState{currentJobs = NewCurrentJobs}};
                false ->
                    %Queue job instead
                    NewQueue = queue:in({AssignmentID,DirID,SessionToken},State#nodeState.queue),
                    spawn(fun() ->
                        file:make_dir("./Handins/" ++ DirID),
                        helper_functions:save_files(Files,"./Handins/" ++ DirID ++ "/"),
                        queue_handin(SessionToken)
                    end),
                    {reply,received,State#nodeState{queue = NewQueue}}
            end;
        error ->
            {reply,{error,noassign},State}
    end;


handle_call(
  {finish_job,{SessionToken,Res}},
  _From,
  State) ->
    %TODO add jobs from queue to running
    %TODO Handle errorhandling with master communication?
    %TODO Kill FSM
    {FilePath, FsmPID} = dict:fetch(SessionToken,State#nodeState.currentJobs),
    helper_functions:delete_dir("./Handins/" ++ FilePath++"/"),
    NewCurrentJobs = dict:erase(SessionToken,State#nodeState.currentJobs),
    master_server:update_handin_job(SessionToken,{finished,Res,node()},State#nodeState.masterNode),
    gen_server:cast({?MODULE,node()},{dequeue_job}),
    {reply, ok, State#nodeState{currentJobs = NewCurrentJobs}};

handle_call(
  {add_assignment,AssignmentID,AssignmentDict,Files}, _From,
  State) ->
    Path = "./Assignments/"++AssignmentID,
    case file:make_dir(Path) of
      Pat when Pat =:= ok; Pat =:= {error,eexist} ->
          ModuleName = dict:fetch("module", AssignmentDict),
          case dict:find(ModuleName, State#nodeState.modules) of
              {ok, Module} ->
                  helper_functions:save_files(Files,Path ++ "/"),
                  {ok, Pid} = gen_assignment:build(Module, AssignmentDict, Path++"/", Files),
                  NewAssignments = dict:store(AssignmentID,{Pid, AssignmentDict},State#nodeState.assignments),
                  {reply, ok, State#nodeState{assignments = NewAssignments}};
              error ->
                  {reply, {error, "Module does not exist"}, State}
          end;
      E ->
        {reply, {error,E},State}
    end;

handle_call({add_module,ModuleName,ModuleBinary}, _From, State) ->
    ModName = atom_to_list(ModuleName),
    Path = "./Modules/" ++ ModName,
    case file:write_file(Path++ ".beam",ModuleBinary) of
        ok ->
            case code:load_abs(Path) of
                {module,Module} ->
                    NewModules = dict:store(ModuleName, Module, State#nodeState.modules),
                    {reply,ok,State#nodeState{modules = NewModules}};
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

start_handin(FsmPID,Module,DirID,SessionToken) ->
    %{Module, AssignDict},DirID,SessionToken) ->
    {ok, Pwd} = file:get_cwd(),
    correct_fsm:start_job(FsmPID,Module, Pwd ++ "/Handins/" ++ DirID ++ "/",SessionToken),
    gen_server:cast({?MODULE,node()},{update_handin_status,running,SessionToken}).


%Just so queueing jobs and saving is in seperate process
queue_handin(SessionToken) ->
    gen_server:cast({?MODULE,node()},{update_handin_status,queued,SessionToken}).

init_handin_fsms(Number) ->
    init_handin_fsms(Number,[]).

init_handin_fsms(0,FsmList) ->
    FsmList;
init_handin_fsms(Number,FsmList) ->
    {ok,FsmPID} = correct_fsm:start_link({node()}),
    init_handin_fsms(Number-1,[{FsmPID,free,none,none} | FsmList]).