-module(assignment_parser).

-export ([parse_assignment/1,parse/1]).

%Parser for assignment parser, a valid config file seperates each entry to be parsed
%seperated by newlines, each entry follows: Entry = value, in the case of entries
%with multiple values it is seperated by commas on 1 line


parse_assignment({AssignmentID,Module}) ->
    {AssignmentID,Module}.


parse(Path) ->
    %TODO check for default vals
    %TODO send back a proper missing req error
    case file:read_file(Path) of
         {ok, Binary} ->
            Content = binary_to_list(Binary),
            Lines = string:tokens(Content, "\n"),
            Dict = populate_dict(Lines, dict:new()),
            Required = dict:is_key("runorder", Dict) and dict:is_key("module", Dict)
            and dict:is_key("assignmentid",Dict),
            if 
                Required -> 
                    NewDict = add_defaults(Dict),
                    {ok, NewDict};
                true ->
                    {error,missing_reqs}
            end;
         Error -> Error
    end.

populate_dict([], Dict) -> Dict;
populate_dict([Line | Lines], Dict) ->
    List = lists:map(fun string:to_lower/1,string:tokens(Line, "=")),
    [Key , Value] = lists:map(fun string:strip/1, List),
    case Key of
        "runorder" ->
            Values = string:tokens(Value, ","),
            Runorder = parse_runorder_files(Values, []), 
            io:format("~p ~n",[Runorder]),
            NewDict = dict:store("runorder",Runorder,Dict),
            populate_dict(Lines,NewDict);
        "required_libs" ->
            Values = string:tokens(Value, ", "),
            io:format("~p ~n",[Values]),
            NewDict = dict:store("required_libs",Values,Dict),
            populate_dict(Lines,NewDict);
        _ ->
            AtomValue = list_to_atom(Value),
            NewDict = dict:store(Key, AtomValue, Dict),
            populate_dict(Lines, NewDict)
    end.

parse_runorder_files([],ValueList) ->
    ValueList;
parse_runorder_files([Value | Values], Parsed) ->
    [Elem | Elems] = string:tokens(Value, " "),
    case Elem of
        "unsafe" ->
            parse_runorder_files(Values,[{unsafe,lists:nth(1,Elems)} | Parsed]);        
        _ ->
            parse_runorder_files(Values,[{safe,Elem} | Parsed])
    end.

add_defaults(Dict) ->
    case file:read_file("./default.conf") of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            Lines = string:tokens(Content, "\n="),
            add_default_values(Lines,Dict);
         Error -> Error
    end.

add_default_values([],Dict) ->
    Dict;
add_default_values([Entry,Value | Rest],Dict) ->
    case dict:is_key(Entry,Dict) of
        true ->
            add_default_values(Rest,Dict);
        false ->
            NewDict = dict:store(Entry,Value,Dict),
            add_default_values(Rest,NewDict)
    end.