-module(gen_assignment).

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
    case Mod:setup(Config, Dir) of
        {error, Msg} ->
            io:format("~p", [Msg]);
        {doCmd, Cmd} ->
            %TODO: ErlExec (spørg Simon)
            lololo;
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
                ok ->
                    From ! ok,
                    exit(done)
            end;
        {run, AssignmentDir, From} ->
            Mod = State#state.module,
            case Mod:run(State#state.config, AssignmentDir) of
                {error, Msg} ->
                    io:format("~p", [Msg]),
                    From ! {error, Msg};
                {ok, Cmd} ->
                    io:format("~p", [Cmd]),
                    spawn(fun () -> runCmd(Cmd, From) end)
            end
    end,
    doLoop(State).

runCmd(Cmd, From) ->
    %TODO: fix
    okay.
