-module(master_server).
-behaviour(gen_server).

-import (node_server, [queue_handin_job/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1,connect_to/2,get_handin_status/2,send_handin/3,
        add_assignment/3,update_handin_job/3,add_module/3, register_socket/2, deregister_socket/2]).

% @version 1.0.0


connect_to(Node,MasterNode) ->
    try gen_server:call({master,MasterNode},{add_node,Node}) of
        _ ->
            ok
    catch
        exit:_ -> {error,nocon}
    end.


start(ConfigFile) ->
    %% @TODO change ConfigFile variable name
    case config_parser:parse(ConfigFile, true) of
        {ok, Config} ->
            Cookie = dict:fetch("Cookie", Config),
            erlang:set_cookie(node(),Cookie),
            gen_server:start_link({local, master}, ?MODULE, [], []);
        {error, Msg} ->
            io:format("~p", [Msg]),
            {error,Msg}
    end.

get_handin_status(SessionToken,MasterNode) ->
    gen_server:call({master,MasterNode},{handin_status,SessionToken}).

send_handin(AssignmentID,Files,MasterNode) ->
    gen_server:call({master,MasterNode},{send_handin,AssignmentID,Files}).

add_assignment(AssignmentConfig,Files,MasterNode) ->
    gen_server:call({master,MasterNode},{add_assignment,AssignmentConfig,Files}).

update_handin_job(SessionToken,NewStatus,MasterNode) ->
    gen_server:cast({master,MasterNode},{update_job,SessionToken,NewStatus}).

add_module(ModuleName,Binary,MasterNode) ->
    gen_server:call({master,MasterNode},{add_module,ModuleName,Binary}).
register_socket(Pid, MasterNode) ->
    gen_server:call({master, MasterNode},{register_socket, Pid}).

deregister_socket(Pid, MasterNode) ->
    gen_server:call({master, MasterNode},{deregister_socket, Pid}).

-record(masterState, {nodes, sessions, assignments, modules, queue, userSockets}).

init([]) ->
    case helper_functions:create_dirs(["./Modules","./Handins","Assignments"]) of
        ok ->
            spawn(fun() -> start_monitor() end),
            {ok, #masterState{
                nodes = dict:new(),
                sessions = dict:new(),
                assignments = dict:new(),
                modules = dict:new(),
                queue = queue:new(),
                userSockets = []
                }
            };
        {error,E} ->
            {stop,{error,E}}
    end.


handle_cast({nodedown,Node}, State) ->
    %TODO should be in its own process
    case dict:find(Node,State#masterState.nodes) of
        {ok,Jobs} ->
            Nodes = nodes(),
            NumNodes = length(Nodes),
            if
                NumNodes > 0 ->
                    DistFun = fun(Session,Accum) ->
                        case dict:find(Session,State#masterState.sessions) of
                            {ok,{AssignID,_,Path}} ->
                                %TODO fix status update
                                Files = helper_functions:load_files_from_dir("./Handins/" ++ Path ++ "/"),
                                Node = lists:nth(random:uniform(NumNodes,Nodes)),
                                Status = queue_handin_job(Node,AssignID,Path,Files,Session),
                                dict:append(Node,Session,Accum);
                            error ->
                                Accum
                        end
                    end,
                    Dict = lists:foldl(DistFun,State#masterState.nodes,Jobs),
                    {noreply,State#masterState{nodes = Dict}};
                true ->
                    NewNodes = dict:erase(Node,State#masterState.nodes),
                    NewQueue = queue:join(State#masterState.queue,queue:from_list(Jobs)),
                    {noreply,State#masterState{queue = NewQueue,nodes = NewNodes}}
            end;
            %NewNodes = lists:foldl(DistFun,[],)
        error ->
            {noreply,State}
    end;

handle_cast({update_job,SessionToken,NewStatus}, State) ->
    lists:map(fun(Pid) -> Pid ! {SessionToken, NewStatus} end, State#masterState.userSockets),
    case dict:is_key(SessionToken,State#masterState.sessions) of
        true ->
            case NewStatus of
                {finished,ReturnVal,Node} ->
                    io:format("Master server job finished ~p \n",[ReturnVal]),
                    {_,_,DirID} = dict:fetch(SessionToken,State#masterState.sessions),
                    helper_functions:delete_dir("./Handins/" ++ DirID ++ "/"),
                    NewSessions = dict:erase(SessionToken,State#masterState.sessions),
                    RemoveFun = fun(List) -> lists:delete(SessionToken,List) end,
                    NewNodes = dict:update(Node,RemoveFun,State#masterState.nodes),
                    {noreply, State#masterState{nodes=NewNodes,sessions=NewSessions}};
                    %{reply, {ok,finished}, State#masterState{nodes=NewNodes,sessions=NewSessions}};
                Status ->
                    NewSessions = dict:update(SessionToken,fun ({X, _, Y}) -> {X, Status, Y} end,State#masterState.sessions),
                    {noreply, State#masterState{sessions=NewSessions}}
                    %{reply, {ok,updated}, State#masterState{sessions=NewSessions}}
            end;
        false ->
            {noreply,State}
            %{reply, {error,nosess}, State}
    end;


%This function dont update the status, should fix
handle_cast({requeue_job,SessionToken,Node}, State) ->
    case dict:find(SessionToken,State#masterState.sessions) of
        {ok,{AssignmentID,_,DirID}} ->
                Files = helper_functions:load_files_from_dir("./Handins/" ++ DirID ++ "/"),
                queue_handin_job(Node,AssignmentID,DirID,Files,SessionToken),
            NewNodes = dict:append(Node,SessionToken, State#masterState.nodes),
            {noreply,State#masterState{nodes = NewNodes}};
        error ->
            {noreply, State}
    end.


handle_call({add_node,Node}, _From, State) ->
    case net_kernel:connect_node(Node) of
        true ->
            spawn(fun() -> send_files_to_node(Node,
                                              State#masterState.assignments,
                                              State#masterState.modules,
                                              State#masterState.queue)
                            end),
            io:format("~p\n\n",[State#masterState.queue]),
            {reply, ok, State#masterState{queue = queue:new()}};
        false ->
            {reply, ok, State};
        ignored ->
            %Is a case of connect node, added with dummy for now
            {reply, ok, State}
    end;

handle_call({send_handin,AssignmentID,Files},_From, State) ->
    case dict:is_key(AssignmentID,State#masterState.assignments) of
        true ->
            %TODO some datastructure where searching in either nodes or session key
            NumberOfNodes = length(nodes()),
            if
                NumberOfNodes > 0 ->
                    SessionToken = make_ref(),
                    io:format("Started session ~p\n",[SessionToken]),
                    Node = lists:nth(random:uniform(NumberOfNodes),nodes()),
                    DirID = create_handin_dirpath(8),
                    file:make_dir("./Handins/" ++ DirID),
                    spawn(fun() -> helper_functions:save_files(Files,"./Handins/" ++ DirID ++ "/") end),
                    %TODO Handle status callback
                    Status = queue_handin_job(Node,AssignmentID,DirID,Files,SessionToken),
                    NewNodes = dict:append(Node,SessionToken,State#masterState.nodes),
                    NewSessions = dict:store(SessionToken,{AssignmentID,Status,DirID},State#masterState.sessions),
                    {reply,{ok,{SessionToken,Status}},State#masterState{nodes = NewNodes, sessions=NewSessions}};
                true ->
                    io:format("Could not start session, no nodes available"),
                    {reply,{error,no_nodes},State}
            end;
        false ->
            io:format("no assignment id matching the given argument"),
            {reply,{error,no_assignment_id},State}
    end;


handle_call({handin_status,SessionToken}, _From, State) ->
    case dict:is_key(SessionToken,State#masterState.sessions) of
        true ->
            %List is only 1 elem long as long as we ensure unique IDs
            [{_,Status}] = dict:fetch(SessionToken, State#masterState.sessions),
            {reply,{ok,Status}, State};
        false ->
            {reply, {error,no_session}, State}
    end;


handle_call({add_assignment,AssignmentConfigBinary,Files}, _From, State) ->
    case assignment_parser:parse(AssignmentConfigBinary) of
        {ok,Dict} ->
            case check_assignment_parameters(Dict,State) of
                ok ->
                    AssignmentID = dict:fetch("assignmentid",Dict),
                    Name = "Test", %TODO: generate assignmentID on server --dict:fecth(""),
                    BroadcastDict = dict:store("id", "AssignmentID", dict:store("name", Name, dict:new())),
                    do_broadcast({newAssignment, BroadcastDict}, State#masterState.userSockets),
                    NewAssignments = dict:store(AssignmentID,Dict,State#masterState.assignments),
                    Path = "./Assignments/" ++ AssignmentID ++ "/",
                    %TODO Error handling
                    file:make_dir(Path),
                    spawn(fun() -> helper_functions:save_files(Files,Path) end),
                    spawn(fun() -> send_assignment_to_node(nodes(),AssignmentID,Dict,Files) end),
                    {reply,{ok,AssignmentID},State#masterState{assignments=NewAssignments}};
                {error,Error} ->
                    {reply, {error,Error},State}
            end;
        {error, Err} ->
            {reply, {error,Err}, State}
    end;


handle_call({register_socket, Pid}, _From, State) ->
    io:format("~p", [Pid]),
    Pid ! {hello, State#masterState.assignments},
    {reply, ok, State#masterState{userSockets = [Pid | State#masterState.userSockets]}};

handle_call({deregister_socket, Pid}, _From, State) ->
    io:format("~p", [Pid]),
    {reply, ok, State#masterState{userSockets = lists:subtract(State#masterState.userSockets, [Pid])}};

handle_call({broadcast, Msg}, _From, State) ->
    io:format("~p", [State#masterState.userSockets]),
    lists:map(fun(Pid) -> Pid ! Msg end, State#masterState.userSockets),
    {reply, ok, State};


handle_call({add_module,ModuleName,Binary}, _From, State) ->
    ModulePath = "./Modules/" ++ atom_to_list(ModuleName) ++ ".beam",
    case file:write_file(ModulePath,Binary) of
        ok ->
            NewModules = dict:store(ModuleName,ModulePath,State#masterState.modules),
            send_module_to_nodes(nodes(),ModuleName,Binary),
            {reply, ok, State#masterState{modules = NewModules}};
        {error, Reason} ->
            {reply,{error,Reason},State}
    end;


handle_call(_Message, _From, State) ->
    io:format("Got error"),
    {reply, error, State}.



handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

check_assignment_parameters(AssignmentDict,State) ->
    Modules = State#masterState.modules,
    Assignments = State#masterState.assignments,
    case dict:is_key(dict:fetch("assignmentid",AssignmentDict),Assignments) of
        true ->
            {error,assidexist};
        false ->
            case dict:is_key(dict:fetch("module",AssignmentDict),Modules) of
                true ->
                    ok;
                false ->
                    {error,nomodule}
        end
    end.

do_broadcast(Msg, Sockets) ->
    lists:map(fun(Pid) -> Pid ! Msg end, Sockets).


send_module_to_nodes(Nodes,ModuleName,ModuleBinary) ->
    UpdateFun = fun(Node) ->
        node_server:add_module(Node,ModuleName,ModuleBinary) end,
    lists:map(UpdateFun,Nodes).


send_assignment_to_node(Nodes,AssignmentID,AssignmentDict,Files) ->
    UpdateFun = fun(Node) ->
        node_server:add_assignment(Node,AssignmentID,AssignmentDict,Files) end,
    lists:map(UpdateFun,Nodes).

send_files_to_node(Node,Assignments,Modules,Queue) ->
    %Modules, Assignments
    AssignmentList = dict:to_list(Assignments),
    ModuleList = dict:to_list(Modules),
    AssignSendFun = fun({AssignmentID,AssignDict}) ->
        Files = helper_functions:load_files_from_dir("./Assignments/" ++ AssignmentID ++ "/"),
        node_server:add_assignment(Node,AssignmentID,AssignDict,Files)
    end,
    ModuleSendFun = fun({ModuleName,ModulePath}) ->
        {ok,Binary} = file:read_file(ModulePath),
        node_server:add_module(Node,ModuleName,Binary)
    end,
    HandinSendFun = fun(SessionToken) ->
        gen_server:cast({master,node()},{requeue_job,SessionToken,Node})
    end,
    lists:map(ModuleSendFun,ModuleList),
    lists:map(AssignSendFun,AssignmentList),
    lists:map(HandinSendFun,queue:to_list(Queue)).



start_monitor() ->
    net_kernel:monitor_nodes(true),
    monitor_loop().

%Simple monitor implementation
monitor_loop() ->
    receive
        {nodedown,Node} ->
            gen_server:cast({master,node()},{nodedown,Node}),
            io:format("Node ~p died \n",[Node]);
        {nodeup,_Node} ->
            nothing
    end,
    monitor_loop().

create_handin_dirpath(Size) ->
    DirID = helper_functions:gen_directory_string(8),
    %Check if dir already exist, create new ID if so
    case file:list_dir("./Handins/" ++ DirID) of
        {ok, _} ->
            create_handin_dirpath(Size);
        _ ->
            DirID
    end.


