-module(start).

-export([start/0, start_server/1, start_server_with_config/1]).

start() ->
    node_server:start("./server.conf").

start_server([Type]) ->
    case string:to_lower(Type) of
        "node" -> node_server:start("./server.conf");
        "master" -> master_server:start("./server.conf");
        _ -> io:format("~p is not a server type. Use either node or server.", [Type])
    end.

start_server_with_config([Type, FilePath]) ->
    case string:to_lower(Type) of
        "node" -> node_server:start(FilePath);
        "master" -> master_server:start(FilePath);
        _ -> io:format("~p is not a server type. Use either node or server.", [Type])
    end.
