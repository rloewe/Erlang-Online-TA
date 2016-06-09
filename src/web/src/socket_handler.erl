-module(socket_handler).
-behaviour(cowboy_websocket_handler).

-import(master_server, [register_socket/2, deregister_socket/2]).

-export([init/3]).
-export([websocket_handle/3, websocket_info/3, websocket_init/3]).
-export([websocket_terminate/3]).

-record(state, {
}).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    register_socket(self(), 'master@127.0.0.1'),
    {ok, Req, #state{}, 60000}.

% We do not expect messages from the client
websocket_handle(_Frame, Req, State) ->
    io:format("~p", [_Frame]),
    {reply, {text, "lol"}, Req, State}.

websocket_info({hello, Assignments}, Req, State) ->
    {reply, {text, json:encode(Assignments)}, Req, State};
websocket_info({newAssignment, Assignment}, Req, State) ->
    {reply, {text, json:encode(Assignment)}, Req, State};
websocket_info(_Message, Req, State) ->
    io:format("~p", [Req]),
    %% Perform post_init initialization here...
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    deregister_socket(self(), 'master@127.0.0.1'),
    ok.
