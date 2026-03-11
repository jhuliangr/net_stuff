-module(ping_pong).

%%% @doc
%%% The canonical distributed Erlang example — two processes exchanging a
%%% message N times, then exiting cleanly.
%%%
%%% This module demonstrates several core Erlang/OTP ideas at once:
%%%
%%%   • Spawning processes and passing Pids as first-class values
%%%   • Selective receive: each clause matches a specific message shape
%%%   • Reference-transparent process location: spawn/4 puts pong on a
%%%     remote node while ping stays local; the message-passing code is
%%%     identical in both cases
%%%   • Graceful termination via a sentinel message ("finish")
%%%
%%% ── Usage ────────────────────────────────────────────────────────────────
%%%
%%%   Local:
%%%     ping_pong:start(5).
%%%
%%%   Distributed (pong on a remote node):
%%%     ping_pong:start_distributed('node2@hostname', 5).
%%%
%%%   Both nodes must share the same Erlang cookie and be connected first
%%%   (see src/genServer/connect.md).
%%%
%%% @end

-export([start/1, start_distributed/2]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn ping and pong on the current node and run Rounds exchanges.
-spec start(Rounds :: pos_integer()) -> ok.
start(Rounds) ->
    PongPid = spawn(fun pong/0),
    spawn(fun() -> ping(PongPid, Rounds) end),
    ok.

%% @doc Spawn pong on RemoteNode; ping stays on the current node.
%%      The rest of the code is unchanged — location transparency in action.
-spec start_distributed(RemoteNode :: node(), Rounds :: pos_integer()) -> ok.
start_distributed(RemoteNode, Rounds) ->
    PongPid = spawn(RemoteNode, fun pong/0),
    spawn(fun() -> ping(PongPid, Rounds) end),
    ok.

%%====================================================================
%% Internal — ping side
%%====================================================================

%% @private Base case: all rounds done.  Tell pong to stop.
ping(PongPid, 0) ->
    io:format("[ping] done after all rounds — sending finish~n"),
    PongPid ! finish;

%% @private Recursive case: send a ping, wait for the pong reply.
ping(PongPid, N) ->
    PongPid ! {ping, self(), N},
    receive
        pong ->
            io:format("[ping] received pong (~p round(s) remaining)~n", [N - 1]),
            ping(PongPid, N - 1)
    after 5000 ->
        io:format("[ping] timed out waiting for pong~n"),
        exit(timeout)
    end.

%%====================================================================
%% Internal — pong side
%%====================================================================

%% @private Wait for a ping, reply, and loop.  Exits on `finish`.
pong() ->
    receive
        {ping, PingPid, N} ->
            io:format("[pong] received ping (~p round(s) left)~n", [N]),
            PingPid ! pong,
            pong();
        finish ->
            io:format("[pong] received finish — exiting~n")
    after 5000 ->
        io:format("[pong] timed out waiting for ping~n"),
        exit(timeout)
    end.
