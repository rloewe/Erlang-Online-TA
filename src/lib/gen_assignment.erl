-module(gen_assignment).

-import(correct_fsm, [job_done/2]).

-callback setup(Config :: term(), WorkingDir :: term()) ->
    {error, Msg :: term()}
    | {doCmd, Cmd :: term()}
    | done.
-callback teardown(WorkingDir :: term()) ->
    {error, Msg :: term()}
    | {doCmd, Cmd :: term()}
    | done.
-callback run(Config :: term(), AssignmentDir :: term()) ->
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
            io:format("Building"),
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
            io:format("Run run run"),
            Mod = State#state.module,
            try
                case Mod:run(State#state.config, AssignmentDir) of
                    {error, Msg} ->
                        io:format("~p", [Msg]),
                        From ! {error, Msg};
                    {ok, Cmd} ->
                        io:format("hello? I run cmd"),
                        io:format("~p", [Cmd]),
                        Pid = spawn(fun () -> getOutput(From, 10000) end),
                        exec:run(Cmd, [{stdout,Pid},{stderr,Pid}])
                end
            catch
                error:network -> From ! {error, "Assignment config error: network"};
                error:disk -> From ! {error, "Assignment config error: disk"}
            end;
        X ->
            io:format("Error: got ~p\n", [X])
    end,
    doLoop(State).

getOutput(From, Timeout) ->
    %TODO: fix timeout
    io:format("hello? I is getOutput"),
    receive
        {_,_,Msg} ->
            job_done(From, {ok, Msg})
    end.
