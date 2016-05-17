-module (master_server_tests).
-include_lib("eunit/include/eunit.hrl").


start_test() ->
    ?assertEqual({ok,pid()},master_server:start("./server.conf")).