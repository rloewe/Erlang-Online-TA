-module(assignment_parser).

-export ([parse_assignment/1,parse/1]).

%A bad implementation of a parser for assignment parser


parse_assignment({AssignmentID,Module}) ->
    {AssignmentID,Module}.


parse(Path) ->
    %TODO check for default vals
    case file:read_file(Path) of
         {ok, Binary} ->
            Content = binary_to_list(Binary),
            Lines = string:tokens(Content, "\n"),
            Dict = populate_dict(Lines, dict:new()),
            Required = dict:is_key("runorder", Dict) and dict:is_key("module", Dict),
            if 
                Required -> 
                    {ok, Dict};
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
