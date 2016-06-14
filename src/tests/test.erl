-module (test).

-export ([test/0,run/0]).

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
    master_server:send_handin("hello",[{"wat.py",Binary}],node()),

run_haskell() ->
    {ok,Binary} = file:read_file("/root/assignment2/Matematik.hs"),
    master_server:send_handin("hello2",[{"Matematik.hs",Binary}],node()).
