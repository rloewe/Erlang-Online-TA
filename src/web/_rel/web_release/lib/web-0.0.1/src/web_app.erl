-module(web_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {
         '_',
         [
          {"/", cowboy_static, {priv_file, web, "static/index.html"}},
          {"/handin", handin_handler, []},
          {"/addAssignment", assignment_handler, []},
          {"/socket", socket_handler, []}

         ]
        }
    ]),
    erlang:set_cookie(node(), herpderpdiderp),
    net_kernel:connect_node('master@127.0.0.1'),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	web_sup:start_link().

stop(_State) ->
	ok.
