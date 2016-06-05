-module(assignment_handler).
-behaviour(cowboy_http_handler).
-import(master_server, [add_assignment/3]).

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
            {ok, [{file, _Filename, Content}]} = dict:find(<<"config">>, Input),
            {ok, Files} = dict:find(<<"files">>, Input),
            Response = add_assignment(Content,
                           'master@127.0.0.1',
                           lists:map(fun ({file, Filename, FileContent}) ->
                                             {binary:bin_to_list(Filename),
                                              FileContent}
                                     end,
                                     Files)),
            case Response of
                {ok, _ID} ->
                    io:format("Good"),
                    {ok, Req4} = cowboy_req:reply(200,
                        [{<<"content-type">>, <<"text/html">>}],
                        <<"Assignment uploaded">>,
                        Req),
                    {ok, Req4, State};
                {error, _Error} ->

                    {ok, Req4} = cowboy_req:reply(200,
                        [{<<"content-type">>, <<"text/html">>}],
                        <<"Error in assignment">>,
                        Req),
                    {ok, Req4, State}
            end;
        {ok, _, _Req2} ->
            {ok, Req3} = cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                <<"Error">>,
                Req),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
