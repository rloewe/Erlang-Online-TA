-module(master_server).
-behaviour(gen_server).
-import (config_parser, [parse_config/1]).    
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1,connect_to/2,get_assignment_status/1,send_assignment/2)]).

connect_to(Node,Specs) ->
    gen_server:call({global,master},{add_node,Node,Specs}).

start(ConfigFile) ->
    %TODO Find a way to add libs to erlang path
    %Cookie = parse_config(ConfigFile),
    Cookie = test,
    erlang:set_cookie(node(),Cookie),
    gen_server:start_link({global, master}, ?MODULE, [], []).

get_assignment_status(SessionToken) ->
    gen_server:call({global,master},{assignment_status,SessionToken}).

send_assignment(AssignmentID,Files) ->
    %Do some magic to deal with files
    gen_server:call({global,master},{send_assignment,AssignmentID,Files}).

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
            NewNodes = Nodes,
            nothing;
        false ->
            case net_kernel:connect_node(Node) of
                true ->
                    NewNodes = dict:append(Node,Specs,Nodes);
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
                    Node = lists:nth(random:uniform(NumberOfNodes),nodes()),
                    %Do some magic with the node
                    NewSessions = dict:append(SessionToken,{AssignmentID,running},Sessions),
                    Reply = {ok,SessionToken};
                true ->
                    NewSessions = Sessions,
                    Reply = {error,no_nodes}
            end;
        false ->
            NewSessions = Sessions,
            Reply = {error,no_assignment_id}
    end, 
    {reply, Reply, {Nodes,NewSessions,Assignments}};


handle_call({assignment_status,SessionToken}, _From, {Nodes,Sessions,Assignments}) ->
    case dict:is_key(SessionToken) of 
        true ->
            {_,Status} = dict:fetch(SessionToken,Sessions),
            Reply = {ok,Status};
        false ->
            Reply = {error,no_session}
    end,
    {reply, Reply, {Nodes,Sessions,Assignments}};

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
