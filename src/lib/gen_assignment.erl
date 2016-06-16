-module(gen_assignment).

-import(correct_fsm, [job_done/2]).

-callback setup(Config :: term(), WorkingDir :: term()) ->
    {error, Msg :: term()}
    | {doCmd, Cmd :: term()}
    | done.
-callback teardown(Config :: term(), WorkingDir :: term()) ->
    {error, Msg :: term()}
    | {doCmd, Cmd :: term()}
    | done.
-callback run(Config :: term(), AssignmentDir :: term(), WorkingDir :: term()) ->
    {error, Msg :: term()}
    | {ok, RunCmd :: term(), StartUpTime :: term()}.

-export([build/4, remove/2, run/3]).

-record(state, {module, config, workingDir, correctionScripts}).

build(Mod, Config, Dir, Files) ->
    Pid = spawn(fun() -> init(Mod, Config, Dir, Files) end),
    {ok, Pid}.

remove(Pid, From) ->
    Pid ! {stop, From}.

run(Pid, AssignmentDir, From) ->
    Pid ! {run, AssignmentDir, From}.

init(Mod, Config, Dir, Files) ->
    helper_functions:save_files(Files, Dir),
    exec:start([root]),
    case Mod:setup(Config, Dir) of
        {error, Msg} ->
            io:format("~p", [Msg]);
        {doCmd, Cmd} ->
            exec:run(Cmd,[]),
            doLoop(#state{
                      module = Mod,
                      config = Config,
                      workingDir = Dir,
                      correctionScripts = Files
                     });
        done ->
            doLoop(#state{
                      module = Mod,
                      config = Config,
                      workingDir = Dir,
                      correctionScripts = Files
                     })
    end.

doLoop(State) ->
    receive
        {stop, From} ->
            Mod = State#state.module,
            case Mod:teardown(State#state.workingDir) of
                {error, Msg} ->
                    io:format("~p", [Msg]),
                    From ! {error, Msg};
                {doCmd, Cmd} ->
                    exec:run(Cmd,[]);
                done ->
                    From ! ok,
                    exit(done)
            end;
        {run, AssignmentDir, From} ->
            Mod = State#state.module,
            WorkingDir = State#state.workingDir,
            try
                case Mod:run(State#state.config, AssignmentDir, WorkingDir) of
                    {error, Msg} ->
                        io:format("~p", [Msg]),
                        From ! {error, Msg};
                    {ok, Cmd, StartUpTime} ->
                        MaxTime = dict:fetch("maxtime", State#state.config) * 1000 + StartUpTime * 1000,
                        spawn(fun() ->
                                      exec:start([root]),
                                      Pid = spawn(fun () -> getOutput(From, "") end),
                                      io:format("~p", [Pid]),
                                      {ok, _, I} = exec:run(Cmd, [{stdout,Pid},{stderr,Pid}, monitor]),
                                      receive
                                          {'DOWN', _, _, _, _} ->
                                              Pid ! stahp
                                      after
                                          MaxTime ->
                                            exec:stop(I),
                                            Pid ! {herp, derp, <<"Execution stopped because it ran out of time">>},
                                            Pid ! stahp
                                      end
                              end);
                    X ->
                        io:format("Run gave: ~p\n", [X]),
                        job_done(From, {error, "Wat"})
                end
            catch
                error:network -> job_done(From, {error, "Assignment config error: network"});
                error:disk -> job_done(From, {error, "Assignment config error: disk"})
            end;
        X ->
            io:format("Error: got ~p\n", [X])
    end,
    doLoop(State).

getOutput(From, Output) ->
    %TODO: fix timeout
    receive
        {_,_,Msg} ->
            getOutput(From, Output ++ binary:bin_to_list(Msg));
        stahp ->
            job_done(From, {ok, Output})
    end.
