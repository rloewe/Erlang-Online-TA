-module(node_server).
-behaviour(gen_server).
-import (master_server, [connect_to/2]).
-import (config_parser, [parse_config/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

start(_ConfigFile) ->
    %TODO add parse config
    {Cookie,MasterNode,Specs} = {test,'master@198.211.122.172',none},
    erlang:set_cookie(node(),Cookie),
    net_kernel:connect_node(MasterNode),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [MasterNode,Specs], []).

init([MasterNode,Specs]) ->
    %master:connect_to(node()),
    case connect_to(node(),Specs) of
        ok ->
            Queue = none,
            Assignments = dict:new(),
            CurrentJobs = [],
            {ok, {Queue,Assignments,CurrentJobs,MasterNode}};
        _ ->
            {stop,nocon}
    end.


handle_cast(_Message, State) ->
    {noreply, State}.



handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
