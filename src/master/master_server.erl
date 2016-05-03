-module(master_server).
-behaviour(gen_server).
-import (config_parser, [parse_config/1]).    
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

start(ConfigFile) ->
    %TODO Find a way to add libs to erlang path
    %Cookie = parse_config(ConfigFile),
    Cookie = test,
    erlang:set_cookie(node(),Cookie),
    gen_server:start_link({global, master}, ?MODULE, [], []).

init([]) ->
    Nodes = dict:new(),
    Session = dict:new(),
    Assignments = dict:new(),
    {ok, {Nodes,Session,Assignments}}.

handle_cast(_Message, State) ->
    {noreply, State}.


handle_call({add_node,Node,Specs}, _From, {Nodes,Session,Assignments}) ->
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
    {reply, ok, {NewNodes}};

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
