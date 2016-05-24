-module(hello_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        <<"<!DOCTYPE html> <html> <head> </head> <body> <form action=\"/form\" method=\"POST\" enctype=\"multipart/form-data\"> <input type=\"text\" name=\"foo\" /> <input type=\"file\" name=\"bar\" /> <input type=\"submit\" /> </form> <form action=\"/form\" method=\"POST\"> <input type=\"text\" name=\"foo\" /> <input type=\"text\" name=\"bar\" /> <input type=\"submit\" /> </form> </body> </html> ">>,
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
