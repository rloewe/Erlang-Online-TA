-module(assignment_handler).
-behaviour(cowboy_http_handler).
-import(master_server, [add_assignment/2]).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, {<<"multipart">>, <<"form-data">>, _}, Req2} ->
            {ok, Input, _Req3} = formdata:multipart(Req2),
            {ok, Config} = dict:find("config", Input),
            add_assignment(Config, 'master@127.0.0.1'),
            {ok, Req4} = cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                <<"Assignment uploaded">>,
                Req),
            {ok, Req4, State};
        {ok, _, _Req2} ->
            {ok, Req3} = cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                <<"Error">>,
                Req),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
