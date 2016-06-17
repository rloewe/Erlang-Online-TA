-module (test).

-export ([add_docker/0,add_haskell/0,run_docker/0,run_haskell/0,run_n_docker/1,run_n_haskell/1,
        no_assignment_test/0,no_module_test/0,bad_assignment_test/0,bad_module_test/0]).

add_docker() ->
    Master = node(),
    add_module(docker,"./lib/docker.beam",Master),
    add_assignment("/root/assignment/assignment.conf",["/root/assignment/lol.py"],Master).

add_haskell() ->
    Master = node(),
    add_module(safehaskell, "./lib/safehaskell.beam", Master),
    add_assignment("/root/assignment2/assignment.conf",["/root/assignment2/Test.hs"],Master).



add_module(Name,Path,Master) ->
    {ok,Binary} = file:read_file(Path),
    master_server:add_module(Name,Binary,Master).

add_assignment(AssignmentConf,FileNames,Master) ->
    {ok,Binary} = file:read_file(AssignmentConf),
    Files = load_files(FileNames,[]),
    master_server:add_assignment(Binary,Files,Master).


load_files([],Files) ->
    Files;
load_files([Path | Paths], Files) ->
    {ok, Binary} = file:read_file(Path),
    load_files(Paths,[{Path,Binary} | Files]).

run_docker() ->
    {ok,Binary} = file:read_file("/root/assignment/handin.py"),
    master_server:send_handin("hello",[{"wat.py",Binary}],node()).

run_n_docker(1) ->
    {ok,Binary} = file:read_file("/root/assignment/handin.py"),
    master_server:send_handin("hello",[{"wat.py",Binary}],node());
run_n_docker(N) ->
    {ok,Binary} = file:read_file("/root/assignment/handin.py"),
    master_server:send_handin("hello",[{"wat.py",Binary}],node()),
    run_n_docker(N-1).

run_haskell() ->
    {ok,Binary} = file:read_file("/root/ass2handin/Matematik.hs"),
    master_server:send_handin("hello2",[{"Matematik.hs",Binary}],node()).

run_n_haskell(1) ->
    {ok,Binary} = file:read_file("/root/ass2handin/Matematik.hs"),
    master_server:send_handin("hello2",[{"Matematik.hs",Binary}],node());
run_n_haskell(N) ->
    {ok,Binary} = file:read_file("/root/ass2handin/Matematik.hs"),
    master_server:send_handin("hello2",[{"Matematik.hs",Binary}],node()),
    run_n_haskell(N-1).



%Tests if stuff is not added yet
no_assignment_test() ->
    {error, no_assignment_id} = run_haskell().

no_module_test() ->
    Master = node(),
    add_assignment("/root/assignment2/assignment.conf",["/root/assignment2/Test.hs"],Master).

%Test that will crash the server
bad_assignment_test() ->
    master_server:add_assignment(<<"Bad file">>,[],node()).

bad_module_test() ->
    master_server:add_module(safehaskell,<<"got you good">>,node()),
    add_assignment("/root/assignment2/assignment.conf",["/root/assignment2/Test.hs"],node()).
