-module(master_server).
-behaviour(gen_server).

-import (node_server, [queue_handin_job/4]).
-import (config_parser, [parse/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1,connect_to/3,get_handin_status/2,send_handin/3,
        add_assignment/3,update_handin_job/3,add_module/3, register_socket/2, deregister_socket/2]).

% @version 1.0.0


connect_to(Node,Specs,MasterNode) ->
    gen_server:call({master,MasterNode},{add_node,Node,Specs}).

start(ConfigFile) ->
    %% @TODO change ConfigFile variable name
    case parse(ConfigFile, true) of
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
    %Do some magic to deal with files
    gen_server:call({master,MasterNode},{send_handin,AssignmentID,Files}).

add_assignment(AssignmentConfig,MasterNode,Files) ->
    gen_server:call({master,MasterNode},{add_assignment,AssignmentConfig,Files}).

update_handin_job(SessionToken,NewStatus,MasterNode) ->
    gen_server:call({master,MasterNode},{update_job,SessionToken,NewStatus}).

add_module(ModuleName,Binary,MasterNode) ->
    gen_server:call({master,MasterNode},{add_module,ModuleName,Binary}).
register_socket(Pid, MasterNode) ->
    gen_server:call({master, MasterNode},{register_socket, Pid}).

deregister_socket(Pid, MasterNode) ->
    gen_server:call({master, MasterNode},{deregister_socket, Pid}).

-record(masterState, {nodes, sessions, assignments, modules, userSockets}).

init([]) ->
    {ok, #masterState{
            nodes = dict:new(),
            sessions = dict:new(),
            assignments = dict:new(),
            modules = dict:new(),
            userSockets = []
           }
    }.

handle_cast(_Message, State) ->
    {noreply, State}.


handle_call({add_node,Node,Specs}, _From, State) ->
    case dict:is_key(Node,State#masterState.nodes) of
        true ->
            %Just adding the case, dont know what to do with it yet
            NewNodes = State#masterState.nodes;
        false ->
            case net_kernel:connect_node(Node) of
                true ->
                    NewNodes = dict:store(Node,Specs,State#masterState.nodes);
                false ->
                    NewNodes = State#masterState.nodes;
                ignored ->
                    %Is a case of connect node, added with dummy for now
                    NewNodes = State#masterState.nodes
            end
        end,
    {reply, ok, State#masterState{nodes=NewNodes}};

handle_call({send_handin,AssignmentID,Files},_From, State) ->
    case dict:is_key(AssignmentID,State#masterState.assignments) of
        true ->
            %Random distribution of work over nodes
            NumberOfNodes = length(nodes()),
            if 
                NumberOfNodes > 0 ->
                    SessionToken = make_ref(),
                    io:format("Started session ~p",[SessionToken]),
                    Node = lists:nth(random:uniform(NumberOfNodes),nodes()),
                    Status = queue_handin_job(Node,AssignmentID,Files,SessionToken),
                    %TODO some magic with the node
                    NewSessions = dict:store(SessionToken,{AssignmentID,Status},State#masterState.sessions),
                    {reply,{ok,{SessionToken,Status}},State#masterState{sessions=NewSessions}};
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
    %TODO Fix sending files in process of its own
    case assignment_parser:parse(AssignmentConfigBinary) of
        {ok,Dict} ->
            case check_assignment_parameters(Dict,State) of
                ok ->
                    %TODO Store files on server?
                    AssignmentID = dict:fetch("assignmentid",Dict),
                    NewAssignments = dict:store(AssignmentID,Dict,State#masterState.assignments),
                    ModuleBinary = dict:fetch(dict:fetch("module",Dict),State#masterState.modules),
                    spawn(send_assignment_to_node(nodes(),AssignmentID,Dict,ModuleBinary,Files)),
                    {reply,ok,State#masterState{assignments=NewAssignments}};
                {error,Err} ->
                    {reply, {error,Err},State}
            end;
        {error, Err} ->
            {reply, {error,Err}, State}
    end;

handle_call({update_job,SessionToken,NewStatus}, _From, State) ->
    lists:map(fun(Pid) -> Pid ! {SessionToken, NewStatus} end, State#masterState.userSockets),
    case dict:is_key(SessionToken,State#masterState.sessions) of
        true ->
            case NewStatus of
                running ->
                    NewSessions = dict:store(SessionToken,NewStatus,State#masterState.sessions),
                    {reply, {ok,updated}, State#masterState{sessions=NewSessions}};
                {finished,ReturnVal} ->
                    %TODO magic with files and return call to end user
                    io:format("Master server job finished ~p \n",[ReturnVal]),
                    NewSessions = dict:erase(SessionToken,State#masterState.sessions),
                    {reply, {ok,finished}, State#masterState{sessions=NewSessions}}
            end;
        false ->
            {reply, {error,nosess}, State}
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
    NewModules = dict:store(ModuleName,Binary,State#masterState.modules),
    {reply, ok, State#masterState{modules = NewModules}};

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


send_assignment_to_node(Nodes,AssignmentID,AssignmentDict,ModuleBinary,Files) ->
    UpdateFun = fun(Node) -> 
        node_server:add_assignment(Node,AssignmentID,AssignmentDict,ModuleBinary,Files) end,  
    lists:map(UpdateFun,Nodes).