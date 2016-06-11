-module (helper_functions).

-export ([create_dirs/1,save_files/2,gen_directory_string/1]).


create_dirs([]) ->
    ok;
create_dirs([Path | Paths]) ->
    case file:make_dir(Path) of
        Pat when Pat =:= ok; Pat =:= {error,eexist} ->
            create_dirs(Paths);
        E ->
            {error,E}
    end.


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