-module (docker_test).

-export ([start/0]).


start() ->
    exec:start([]),
    Cmd = " echo \"no\"",
    Pid = spawn(fun() -> dmonitor(self(),Cmd) end),
    {ok,_,I} = exec:run("docker run -t -d ubuntu:14.04 /bin/bash",
                                         [{stdout,Pid},{stderr,Pid}]).
    % receive
    %     stop ->
    %         io:format("dddd~n"),
    %         exec:stop(I) 
    % end.


dmonitor(MasterPid,Cmd) ->
    io:format("test~n"),
    receive
        {_,_,M} ->
            Docker_string = binary:bin_to_list(M),
            [DockerID] = string:tokens(Docker_string,"<>\n"),
            StartString = "docker exec ",
            InputString = "docker exec " ++ DockerID ++ Cmd,
            io:format("~p ~n",[InputString]),
            %io:format("~p ~n",[InputString]),
            D = exec:run(InputString,[{stdout,print},{stderr,print}]),
            MasterPid ! {stop}
    after 
        5000 ->
            io:format("No ~n")
    end.



