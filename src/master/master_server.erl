-module(master_server).
-behaviour(gen_server).

export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    gen_server:start_link({global, master}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_cast(Message, State) ->
    {noreply, State}.

handle_call(Message, From, State) ->
    {reply, error, State}.

handle_info(Message, State) -> {noreply, State}.
terminate(Reason, State) -> ok.
code_change(OldVersion, State, Extra) -> {ok, State}.
