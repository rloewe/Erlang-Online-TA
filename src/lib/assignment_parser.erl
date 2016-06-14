-module(assignment_parser).

-export ([parse_assignment/1,parse/1,to_type/1]).

%Parser for assignment parser, a valid config file seperates each entry to be parsed
%seperated by newlines, each entry follows: Entry = value, in the case of entries
%with multiple values it is seperated by commas on 1 line


parse_assignment({AssignmentID,Module}) ->
    {AssignmentID,Module}.


parse(Binary) ->
    %TODO check for default vals
    %TODO send back a proper missing req error
    Content = binary_to_list(Binary),
    Lines = string:tokens(Content, "\n"),
    Dict = populate_dict(Lines, dict:new()),
    Required = check_requirements(["runorder","module","assignmentid"],Dict),
    case Required of
        true ->
            NewDict = add_defaults(Dict),
            {ok, NewDict};
        {false,Req} ->
            {error,missing_req,Req}
    end.

populate_dict([], Dict) -> Dict;
populate_dict([Line | Lines], Dict) ->
    List = lists:map(fun string:to_lower/1,string:tokens(Line, "=")),
    [Key , Value] = lists:map(fun string:strip/1, List),
    case Key of
        "runorder" ->
            Values = string:tokens(Value, ","),
            Runorder = parse_runorder_files(Values, []),
            NewDict = dict:store("runorder",Runorder,Dict),
            populate_dict(Lines,NewDict);
        "required_libs" ->
            Values = string:tokens(Value, ", "),
            NewDict = dict:store("required_libs",Values,Dict),
            populate_dict(Lines,NewDict);
        "module" ->
            NewDict = dict:store("module",list_to_atom(Value),Dict),
            populate_dict(Lines,NewDict);
        _ ->
            NewDict = dict:store(Key, Value, Dict),
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
            Lines = string:tokens(Content, "\n= "),
            add_default_values(Lines,Dict);
         Error -> Error
    end.

add_default_values([],Dict) ->
    Dict;
add_default_values([Entry,Value | Rest],Dict) ->
    Fun = to_type(Entry),
    case dict:is_key(Entry,Dict) of
        true ->
            Elem = dict:fetch(Entry,Dict),
            io:format("~p\n",[Elem]),
            NewDict = dict:store(Entry,Fun(Elem),Dict),
            add_default_values(Rest,NewDict);
        false ->
            NewDict = dict:store(Entry,Fun(Value),Dict),
            add_default_values(Rest,NewDict)
    end.

to_type(Entry) ->
    case Entry of
        "module" ->
            fun(Elem) -> io:format("hi"),list_to_atom(Elem) end;
        "maxmem" ->
            fun(Elem) -> {E,_} = string:to_integer(Elem), E end;
        "maxtime" ->
            fun(Elem) -> {E,_} = string:to_integer(Elem), E end;
        "network" ->
            fun(Elem) -> list_to_atom(Elem) end;
        "disk" ->
            fun(Elem) -> list_to_atom(Elem) end;
        _ ->
            fun(Elem) -> Elem end
    end.

check_requirements([],_) ->
    true;
check_requirements([Requirement | Requirements], Dict) ->
    case dict:is_key(Requirement,Dict) of
        true ->
            check_requirements(Requirements,Dict);
        false ->
            {false,Requirement}
    end.