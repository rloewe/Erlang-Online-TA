-module (helper_functions).

-export ([create_dirs/1,save_files/2,gen_directory_string/1,delete_dir/1]).


create_dirs([]) ->
    ok;
create_dirs([Path | Paths]) ->
    case file:make_dir(Path) of
        Pat when Pat =:= ok; Pat =:= {error,eexist} ->
            create_dirs(Paths);
        E ->
            {error,E}
    end.

delete_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok,Files} ->
            delete_files(Files),
            file:del_dir(Dir),
            ok;
        _ ->
            ok
    end.

delete_files([]) ->
    ok;
delete_files([Path | Paths]) ->
    file:delete(Path),
    delete_files(Paths).



save_files([],_) ->
    ok;
save_files([{FileName,File} | Rest ],Path) ->
    file:write_file(Path++FileName,File),
    save_files(Rest,Path).


gen_directory_string(Size) ->
    Allowed = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    NumberOfChars = length(Allowed),
    FoldFun = fun(_,Accum) ->
        [lists:nth(random:uniform(NumberOfChars),Allowed)] ++ Accum end,
    lists:foldl(FoldFun,[],lists:seq(1,Size)).