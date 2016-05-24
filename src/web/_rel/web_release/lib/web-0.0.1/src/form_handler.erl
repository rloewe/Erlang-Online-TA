-module(form_handler).
-behaviour(cowboy_http_handler).
-import(master_server, [send_handin/3]).

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
            {ok, Input, _Req3} = multipart(Req2, dict:new()),
            %io:format("~p", [Input]),
            %send_handin(id, Input,'master@127.0.0.1'),
            gen_server:call({master,'master@127.0.0.1'},{send_handin,lolol,Input}),
            {ok, Req4} = cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                <<"Multipart">>,
                Req),
            {ok, Req4, State};
        {ok, _, _Req2} ->
            {ok, Req3} = cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                <<"Not multipart">>,
                Req),
            {ok, Req3, State}
    end.

multipart(Req, Dict) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            {Req4, UpdatedDict} = case cow_multipart:form_data(Headers) of
                       {data, FieldName} ->
                           {ok, Body, Req3} = cowboy_req:part_body(Req2),
                           {Req3, dict:store(FieldName, {input, Body}, Dict)};
                       {file, FieldName, Filename, _CType, _CTransferEncoding} ->
                           {ok, FileContent, Req3} = stream_file(Req2, <<>>),
                           {Req3, dict:store(FieldName, {file, Filename, FileContent}, Dict)}
                   end,
            multipart(Req4, UpdatedDict);
        {done, Req2} ->
            {ok, Dict, Req2}
    end.

stream_file(Req, BodyIn) ->
    case cowboy_req:part_body(Req) of
        {ok, Body, Req2} ->
            {ok, <<BodyIn/binary, Body/binary>>, Req2};
        {more, Body, Req2} ->
            stream_file(Req2, <<BodyIn/binary, Body/binary>>)
    end.

terminate(_Reason, _Req, _State) ->
    ok.
