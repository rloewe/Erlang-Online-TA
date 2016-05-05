-module (config_parser).
-export ([parse/2]).

parse(Path, IsMaster) ->
    case file:read_file(Path) of
         {ok, Binary} ->
            Content = binary_to_list(Binary),
            Lines = string:tokens(Content, "\n"),
            Dict = populate_dict(Lines, dict:new()),
            %io:format("~p", [populate_dict(Lines, dict:new())]),
            HasKeys = has_required_keys(Dict, IsMaster),
            if
                HasKeys ->
                    {ok, Dict};
                true ->
                    {error, required_keys_missing}
            end;
         Error -> Error
    end.

populate_dict([], Dict) -> Dict;
populate_dict([Line | Lines], Dict) ->
    List = string:tokens(Line, "="),
    [Key, Value] = lists:map(fun string:strip/1, List),
    AtomValue = list_to_atom(Value),
    NewDict = dict:store(Key, AtomValue, Dict),
    populate_dict(Lines, NewDict).

has_required_keys(Dict, IsMaster) ->
    if
        IsMaster ->
            dict:is_key("Cookie", Dict);
        true ->
            dict:is_key("Master", Dict) and dict:is_key("Cookie", Dict)
    end.
