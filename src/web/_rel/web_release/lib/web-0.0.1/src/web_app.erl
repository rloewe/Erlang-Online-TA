-module(web_app).
-behaviour(application).

-import(config_parser, [parse/2]).

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
    io:format("~p", [file:get_cwd()]),
    case parse("server.conf", false) of
        {ok, Config} ->
            Cookie = dict:fetch("Cookie", Config),
            MasterNode = dict:fetch("Master", Config),
            erlang:set_cookie(node(),Cookie),
            net_kernel:connect_node(MasterNode),
            cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                [{env, [{dispatch, Dispatch}]}]
            ),
            web_sup:start_link();
        {error, Msg} ->
            io:format("~p", [Msg]),
            {error,Msg}
    end.

stop(_State) ->
    ok.
