-module(master_server).
-behaviour(gen_server).

-import (assignment_parser, [parse_assignment/1]).
-import (node_server, [queue_assignment_job/4]).
-import (config_parser, [parse/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1,connect_to/3,get_assignment_status/2,send_assignment/3,
        add_assignment/2,assignment_job_updated/3]).

connect_to(Node,Specs,MasterNode) ->
    gen_server:call({master,MasterNode},{add_node,Node,Specs}).

start(ConfigFile) ->
    case parse(ConfigFile, true) of
        {ok, Config} ->
            Cookie = dict:fetch("Cookie", Config),
            erlang:set_cookie(node(),Cookie),
            gen_server:start_link({global, master}, ?MODULE, [], []);
        {error, Msg} ->
            io:format("~p", [Msg])
    end.

get_assignment_status(SessionToken,MasterNode) ->
    gen_server:call({master,MasterNode},{assignment_status,SessionToken}).

send_assignment(AssignmentID,Files,MasterNode) ->
    %Do some magic to deal with files
    gen_server:call({master,MasterNode},{send_assignment,AssignmentID,Files}).

add_assignment(AssignmentConfig,MasterNode) ->
    gen_server:call({master,MasterNode},{add_assignment,AssignmentConfig}).

assignment_job_updated(SessionToken,NewStatus,MasterNode) ->
    gen_server:call({master,MasterNode},{update_job,SessionToken,NewStatus}).

init([]) ->
    Nodes = dict:new(),
    Session = dict:new(),
    Assignments = dict:new(),
    {ok, {Nodes,Session,Assignments}}.

handle_cast(_Message, State) ->
    {noreply, State}.


handle_call({add_node,Node,Specs}, _From, {Nodes,Sessions,Assignments}) ->
    case dict:is_key(Node,Nodes) of
        true ->
            %Just adding the case, dont know what to do with it yet
            NewNodes = Nodes;
        false ->
            case net_kernel:connect_node(Node) of
                true ->
                    NewNodes = dict:store(Node,Specs,Nodes);
                false ->
                    NewNodes = Nodes;
                ignored ->
                    %Is a case of connect node, added with dummy for now
                    NewNodes = Nodes
            end
        end,
    {reply, ok, {NewNodes,Sessions,Assignments}};

handle_call({send_assignment,AssignmentID,Files},_From, {Nodes,Sessions,Assignments}) ->
    case dict:is_key(AssignmentID,Assignments) of
        true ->
            %Random distribution of work over nodes
            NumberOfNodes = length(nodes()),
            if 
                NumberOfNodes > 0 ->
                    SessionToken = make_ref(),
                    io:format("Started session ~p",[SessionToken]),
                    Node = lists:nth(random:uniform(NumberOfNodes),nodes()),
                    Status = queue_assignment_job(Node,AssignmentID,Files,SessionToken),
                    %TODO some magic with the node
                    NewSessions = dict:store(SessionToken,{AssignmentID,Status},Sessions),
                    {reply,{ok,{SessionToken,Status}},{Nodes,NewSessions,Assignments}};
                true ->
                    io:format("Could not start session, no nodes available"),
                    {reply,{error,no_nodes},{Nodes,Sessions,Assignments}}
            end;
        false ->
            io:format("no assignment id matching the given argument"),
            {reply,{error,no_assignment_id},{Nodes,Sessions,Assignments}}
    end;


handle_call({assignment_status,SessionToken}, _From, {Nodes,Sessions,Assignments}) ->
    case dict:is_key(SessionToken,Sessions) of 
        true ->
            %List is only 1 elem long as long as we ensure unique IDs
            [{_,Status}] = dict:fetch(SessionToken,Sessions),
            {reply,{ok,Status},{Nodes,Sessions,Assignments}};
        false ->
            {reply, {error,no_session}, {Nodes,Sessions,Assignments}}
    end;


handle_call(
  {add_assignment,AssignmentConfig},
  _From, 
  {Nodes,Sessions,Assignments}) ->
    AssignmentID = parse_assignment(AssignmentConfig),
    case dict:is_key(AssignmentID,Assignments) of 
        true ->
            {reply,{error,assignment_exist},{Nodes,Sessions,Assignments}};
        false ->
            NewAssignments = dict:store(AssignmentID,none,Assignments),
            {reply, ok, {Nodes,Sessions,NewAssignments}}
    end;


handle_call({update_job,SessionToken,NewStatus}, _From, {Nodes,Sessions,Assignments}) ->
    case dict:is_key(SessionToken,Sessions) of
        true ->
            case NewStatus of 
                running ->
                    NewSessions = dict:store(SessionToken,NewStatus,Sessions),
                    {reply, {ok,updated}, {Nodes,NewSessions,Assignments}};
                {finished,ReturnVal} ->
                    %TODO magic with files and return call to end user
                    NewSessions = dict:erase(SessionToken,Sessions),
                    {reply, {ok,finished}, {Nodes,NewSessions,Assignments}}
            end;
        false ->
            {reply, {error,nosess}, {Nodes,Sessions,Assignments}}
    end;


handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
