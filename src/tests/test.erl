-module (test).

-export ([test/0,run/0]).

test() ->
    Master = node(),
    add_module(docker,"./lib/docker.beam",Master),
    add_assignment("/root/assignment/assignment.conf",["/root/assignment/lol.py"],Master).

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

run() ->
    {ok,Binary} = file:read_file("/root/assignment/handin.py"),
    master_server:send_handin("hello",[{"wat.py",Binary}],node()).