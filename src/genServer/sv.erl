-module(sv).

%%% @doc
%%% Supervisor template — all commonly tweaked knobs, with inline notes.
%%%
%%% A supervisor's only job is to watch child processes and restart them
%%% when they crash, according to the strategy and intensity limits below.
%%% It is a pure OTP behaviour: you describe *what* to supervise and *how*,
%%% and the framework handles process linking, exit signal trapping, and the
%%% restart loop.
%%%
%%% ── Restart strategies ───────────────────────────────────────────────────
%%%
%%%   one_for_one        Restart only the crashed child.  Use when children
%%%                      are independent of one another.
%%%
%%%   one_for_all        Restart every child when any one crashes.  Use when
%%%                      children are tightly coupled and must start/stop
%%%                      as a unit (e.g. a TCP listener + connection pool).
%%%
%%%   rest_for_one       Restart the crashed child AND every child that was
%%%                      started after it.  Use when later children depend on
%%%                      earlier ones being alive.
%%%
%%%   simple_one_for_one Dynamic pool of identical children; superseded by
%%%                      DynamicSupervisor in OTP 21+.
%%%
%%% ── Intensity and period ─────────────────────────────────────────────────
%%%
%%%   The supervisor allows at most `intensity` restarts within any `period`-
%%%   second window.  Exceeding the threshold causes the supervisor itself to
%%%   crash and propagate the failure up the tree — the right behaviour,
%%%   because a restart loop usually means the bug is in init, not runtime.
%%%
%%% @end

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{
        strategy  => one_for_one,   % see module doc for all four options
        intensity => 10,            % max 10 restarts …
        period    => 60             % … within any 60-second window
    },

    %% Each map in ChildSpecs describes one managed child process.
    %%
    %%   id       — unique atom; used by the supervisor internally
    %%   start    — {Module, Function, Args} called to spawn the child
    %%   restart  — permanent  : always restart (even after normal exit)
    %%              transient  : restart only on abnormal exit
    %%              temporary  : never restart
    %%   shutdown — ms to wait for graceful termination before brutal kill
    %%              (use `brutal_kill` to skip the grace period entirely)
    %%   type     — worker | supervisor  (affects shutdown order in trees)
    %%   modules  — list of modules for hot-code upgrade release handling;
    %%              use [dynamic] when the child is a fun/closure
    ChildSpecs = [
        #{
            id       => some_worker,
            start    => {some_worker, start_link, []},
            restart  => permanent,
            shutdown => 2000,
            type     => worker,
            modules  => [some_worker]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
