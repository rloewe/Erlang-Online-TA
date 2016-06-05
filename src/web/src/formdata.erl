-module(formdata).

-export([multipart/1]).

multipart(Req) ->
    multipartWithDict(Req, dict:new()).

multipartWithDict(Req, Dict) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            {Req4, UpdatedDict} = case cow_multipart:form_data(Headers) of
                       {data, FieldName} ->
                           {ok, Body, Req3} = cowboy_req:part_body(Req2),
                           {Req3, dict:append(FieldName, {input, Body}, Dict)};
                       {file, FieldName, Filename, _CType, _CTransferEncoding} ->
                           {ok, FileContent, Req3} = stream_file(Req2, <<>>),
                           {Req3, dict:append(FieldName, {file, Filename, FileContent}, Dict)}
                   end,
            multipartWithDict(Req4, UpdatedDict);
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
