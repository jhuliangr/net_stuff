-module(udp_sender).

%%% @doc
%%% UDP sender — connects to the receiver hub and participates in the
%%% fan-out broadcast.
%%%
%%% ── Port discovery ───────────────────────────────────────────────────────
%%%
%%%   UDP sockets need a local port to receive replies.  Rather than
%%%   requiring the caller to pick one, find_port/1 probes ports starting
%%%   at ServerPort+1 and returns the first one the OS reports as available.
%%%   This keeps the API to a single argument (the receiver's port).
%%%
%%% ── Registration protocol ────────────────────────────────────────────────
%%%
%%%   Senders self-register: the first datagram sent to the receiver causes
%%%   it to add the sender's port to its client map.  No explicit handshake.
%%%
%%%   To deregister cleanly, stop/0 sends the binary `"out"` before closing
%%%   the socket, letting the receiver remove the port immediately rather
%%%   than discovering the dead socket on the next fan-out.
%%%
%%% @end

%%  API
-export([start/1, send/1, stop/0]).

%%  Hardcoded to localhost; change to the receiver's IP for cross-machine use.
-define(SERVER_HOST, {127, 0, 0, 1}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the sender, auto-discovering a free local port.
%%      The sender process is registered as `sender` so send/1 and stop/0
%%      can reach it without holding a Pid.
-spec start(ServerPort :: inet:port_number()) -> true.
start(ServerPort) ->
    Port = find_port(ServerPort + 1),
    register(sender, spawn(fun() -> listen(Port, ServerPort) end)).

%% @doc Send Msg to the receiver, which will fan it out to all other senders.
-spec send(Msg :: iodata()) -> ok.
send(Msg) ->
    sender ! {send, Msg},
    ok.

%% @doc Deregister from the receiver and close the local socket.
-spec stop() -> ok.
stop() ->
    sender ! stop,
    ok.

%%====================================================================
%% Private
%%====================================================================

%% @private Probe ports starting at Port until one is free.
%%           Opens and immediately closes a temporary socket to verify
%%           availability — avoids TOCTOU races with other processes.
find_port(Port) ->
    case gen_udp:open(Port, [binary, {active, false}]) of
        {ok, Socket} ->
            io:format("[udp_sender] using local port ~p~n", [Port]),
            gen_udp:close(Socket),
            Port;
        {error, _} ->
            io:format("[udp_sender] port ~p in use, trying next~n", [Port]),
            find_port(Port + 1)
    end.

%% @private Open the chosen local port and enter the receive loop.
listen(Port, ServerPort) ->
    {ok, Socket} = gen_udp:open(Port, [{active, true}, {mode, binary}]),
    io:format("[udp_sender] listening on port ~p, receiver at ~p~n", [Port, ServerPort]),
    loop(Socket, ServerPort).

%% @private Receive loop.  Handles outgoing sends, incoming fan-out
%%           datagrams from the receiver, and the stop signal.
loop(Socket, ServerPort) ->
    receive
        %% Outgoing message — forward to the receiver hub.
        {send, Msg} ->
            try
                gen_udp:send(Socket, ?SERVER_HOST, ServerPort, Msg)
            catch
                Err ->
                    logger:error("[udp_sender] send failed: ~p~n", [Err])
            end,
            loop(Socket, ServerPort);

        %% Incoming fan-out from the receiver (another sender's message).
        {udp, Socket, Host, ReceiverPort, Data} ->
            io:format("[udp_sender] from ~p:~p -> ~p~n", [Host, ReceiverPort, Data]),
            loop(Socket, ServerPort);

        %% Graceful shutdown: notify the receiver before closing.
        stop ->
            try
                gen_udp:send(Socket, ?SERVER_HOST, ServerPort, "out")
            catch
                Err ->
                    logger:error("[udp_sender] error sending deregister: ~p~n", [Err])
            end,
            gen_udp:close(Socket),
            exit(self(), shutdown);

        %% ICMP port-unreachable (receiver may have restarted).
        {udp_error, Socket, econnreset} ->
            logger:error("[udp_sender] connection reset, receiver may be down~n"),
            loop(Socket, ServerPort);

        Other ->
            io:format("[udp_sender] unexpected message: ~p~n", [Other]),
            loop(Socket, ServerPort)
    end.
