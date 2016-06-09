-module(docker).

-export([setup/2, teardown/2, run/2]).

setup(Config, WorkingDir) ->
    {ok, Id} = dict:find("assignmentid", Config),
    file:write_file(WorkingDir ++ ".dockerignore", "Dockerfile"),
    {ok, Libs} = dict:find("required_lib", Config),
    {ok, RunScripts} = dict:find("runorder", Config),
    file:write_file(
      WorkingDir ++ "Dockerfile",
      "FROM ubuntu\n" ++
      "RUN apt-get update && apt-get install -y " ++ string:join(Libs, " ") ++ "\n" ++
      "RUN useradd -m -d /home/correction correction\n" ++
      "USER correction\n" ++
      "WORKDIR /home/correction\n" ++
      "COPY " ++ WorkingDir ++ " /home/correction\n" ++
      "CMD " ++ string:join(lists:map(fun (Elm) -> "./" ++ Elm end, RunScripts), " && ")
     ),
    {doCmd, "docker build -t " ++ Id ++ " " ++ WorkingDir}.


teardown(Config, WorkingDir) ->
    done.

run(Config, AssignmentDir) ->
    {ok, Id} = dict:find("assignmentid", Config),
    {ok, "docker run -t " ++ Id ++ " -v " ++ AssignmentDir ++ ":/home/correction/handin", 0}.
