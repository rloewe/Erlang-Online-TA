-module(json).

-export([encode/1]).

encode({dict,  _, _, _, _, _, _, _, _} = Dict) ->
    "{" ++ dictelms_to_json(dict:to_list(Dict)) ++ "}";
encode(Elm) ->
    case io_lib:printable_unicode_list(Elm) of
        true ->
            "\"" ++ Elm ++ "\"";
        false ->
            case is_list(Elm) of
                true -> list_to_json(Elm);
                false ->
                    case is_atom(Elm) of
                        true -> "\"" ++ lists:flatten(io_lib:write(Elm)) ++ "\"";
                        false -> lists:flatten(io_lib:write(Elm))
                    end
            end
    end.

list_to_json([]) -> "[]";
list_to_json([Elm | Rest]) ->
    List = lists:map(fun(X) -> encode(X) end, [Elm | Rest]),
    "[" ++ string:join(List, ", ") ++ "]".

% If key is a number, then it will create invalid json
dictelms_to_json([]) -> "";
dictelms_to_json([{Key, Data} | []]) ->
    encode(Key) ++ ": " ++ encode(Data);
dictelms_to_json([{Key, Data} | Rest]) ->
    encode(Key) ++ ": " ++ encode(Data) ++ "," ++ dictelms_to_json(Rest).
