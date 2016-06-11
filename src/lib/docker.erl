-module(docker).

-export([setup/2, teardown/2, run/2]).

setup(Config, WorkingDir) ->
    {ok, Id} = dict:find("assignmentid", Config),
    file:write_file(WorkingDir ++ ".dockerignore", "Dockerfile"),
    {ok, Libs} = dict:find("required_libs", Config),
    {ok, RunScripts} = dict:find("runorder", Config),
    file:write_file(
      WorkingDir ++ "Dockerfile",
      list_to_binary("FROM ubuntu\n" ++
      "RUN apt-get update && apt-get install -y " ++ string:join(Libs, " ") ++ "\n" ++
      "RUN useradd -m -d /home/correction correction\n" ++
      "RUN chown correction:correction -R /home/correction" ++
      "COPY . /home/correction\n" ++
      "USER correction\n" ++
      "WORKDIR /home/correction\n" ++
      "CMD " ++ string:join(lists:map(fun ({_,Elm}) -> "./" ++ Elm end, RunScripts), " && "))
     ),
    {doCmd, "docker build -t " ++ Id ++ " " ++ WorkingDir}.


teardown(Config, WorkingDir) ->
    {ok, Id} = dict:find("assignmentid", Config),
    {doCmd, "docker rmi " ++ Id}.

run(Config, AssignmentDir) ->
    {ok, Id} = dict:find("assignmentid", Config),
    case dict:find("network", Config) of
        {ok, enabled} ->
            Net = "";
        {ok, disabled} ->
            Net = "--net=none ";
        _ ->
            Net = none,
            erlang:error(network)
    end,
    case dict:find("disk", Config) of
        {ok, enabled} ->
            Disk = "";
        {ok, disabled} ->
            Disk = "--read-only ";
        _ ->
            Disk = none,
            erlang:error(disk)
    end,
    {ok, "docker run --rm -v " ++ AssignmentDir ++ ":/home/correction/handin " ++ Net ++ Disk ++ "-t " ++ Id, 0}.
