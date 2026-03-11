-module(tcp_server).

%%% @doc
%%% Multi-client TCP server.
%%%
%%% Each accepted connection gets its own Erlang process — the idiomatic
%%% answer to the C10k problem in Erlang.  Two storage backends are used:
%%%
%%%   • `otp` (custom OTP store, registered as `db`)
%%%       Holds the live client registry: Port → Socket.  Using the custom
%%%       OTP module here shows how it integrates with real networking code.
%%%
%%%   • `server` (gen_server)
%%%       Holds the listener socket so stop/1 can retrieve and close it
%%%       without threading it through every function.
%%%
%%% ── Concurrency model ───────────────────────────────────────────────────
%%%
%%%   start/1 spawns the listener loop.
%%%   accepter/1 blocks on gen_tcp:accept/1; on each new connection it
%%%   spawns a *new* accepter process (so it can immediately accept the
%%%   next client) and then drops into loop/3 for the connected socket.
%%%
%%% ── Message protocol ────────────────────────────────────────────────────
%%%
%%%   {tcp, Socket, Data}      — data received from a client
%%%   {tcp_closed, Socket}     — client disconnected gracefully
%%%   {error, closed}          — client disconnected abruptly
%%%   {msg_for_clients, Msg}   — broadcast request from the server side
%%%   stop                     — shutdown signal from stop/1
%%%
%%% @end

-export([start/1, stop/1, message/2]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the TCP server on Port.
%%      Returns the Pid of the listener process; keep it to call stop/1
%%      and message/2 later.
-spec start(Port :: inet:port_number()) -> pid().
start(Port) ->
    %% The OTP store holds the live client map (Port → Socket).
    %% It is registered as `db` so every per-client process can reach it.
    register(db, otp:start()),
    %% The gen_server holds the listener socket for retrieval in stop/1.
    server:start_link(),
    spawn(fun() -> listen(Port) end).

%% @doc Broadcast Msg to every currently connected client.
%%      The message is delivered to the listener Pid, which fans it out
%%      to each socket stored in the client registry.
-spec message(Pid :: pid(), Msg :: iodata()) -> ok.
message(Pid, Msg) ->
    Pid ! {msg_for_clients, Msg},
    ok.

%% @doc Gracefully shut down the server.
%%      Sends a stop signal to the listener, closes all client sockets,
%%      unregisters the client registry, closes the listener socket, and
%%      terminates the gen_server.
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    Pid ! stop,
    %% Retrieve and close every active client socket.
    {Map} = otp:get_data(db),
    maps:foreach(fun(Port, Socket) ->
                    otp:delete(db, Port),
                    gen_tcp:close(Socket)
                 end,
                 Map),
    unregister(db),
    %% Close the listener socket so the OS port is freed immediately.
    gen_tcp:close(server:get(socket)),
    server:stop().

%%====================================================================
%% Private
%%====================================================================

%% @private Open the listening socket and hand off to the accept loop.
listen(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{active, true}, {mode, binary}]),
    logger:notice("TCP server started on port ~p~n", [Port]),
    server:store(socket, Socket),
    accepter(Socket).

%% @private Block until a client connects, then spawn a fresh accepter
%%           process (so this one can accept the next client immediately)
%%           before dropping into the per-connection receive loop.
accepter(SocketListener) ->
    try
        {ok, Socket} = gen_tcp:accept(SocketListener),
        %% Spawn a sibling to keep accepting while we handle this client.
        spawn(?MODULE, accepter, [SocketListener]),
        {ok, {Host, Port}} = inet:peername(Socket),
        otp:save(db, Port, Socket),
        io:format("Client connected: ~p on port ~p~n", [Host, Port]),
        loop(Socket, Host, Port)
    catch
        %% The listener socket was closed (e.g. stop/1 was called).
        error:{badmatch, {error, closed}} ->
            io:format("[tcp_server] listener socket closed, accepter exiting~n")
    end.

%% @private Per-connection receive loop.  Handles all message types that
%%           gen_tcp delivers in `{active, true}` mode.
loop(Socket, Host, Port) ->
    receive
        %% Client sent a message beginning with "exit" — treat it as a
        %% disconnect request and close the socket gracefully.
        {tcp, Socket, <<"exit", _/binary>>} ->
            logger:notice("Client ~p (port ~p) requested disconnect~n", [Host, Port]),
            otp:delete(db, Port),
            gen_tcp:close(Socket);

        %% Normal data message — echo it back with a server-side wrapper.
        {tcp, Socket, Msg} ->
            logger:notice("Received from ~p (port ~p): ~p~n", [Host, Port, Msg]),
            gen_tcp:send(Socket, [<<"Server received: ">>, Msg]),
            loop(Socket, Host, Port);

        %% The client closed the connection from its side.
        {tcp_closed, _Socket} ->
            otp:delete(db, Port),
            logger:notice("Client ~p disconnected, port ~p is free~n", [Host, Port]);

        %% Broadcast request: deliver Msg to every registered client socket.
        {msg_for_clients, Msg} ->
            {Map} = otp:get_data(db),
            maps:foreach(fun(_K, V) -> gen_tcp:send(V, [Msg]) end, Map),
            loop(Socket, Host, Port);

        %% Abrupt disconnect (e.g. network error on an already-active socket).
        {error, closed} ->
            otp:delete(db, Port),
            io:format("[tcp_server] client ~p abruptly disconnected (port ~p)~n",
                      [Host, Port]);

        %% Shutdown signal from stop/1 — close this connection and exit.
        stop ->
            gen_tcp:close(Socket),
            ok;

        Other ->
            logger:warning("Unexpected message from ~p: ~p~n", [Host, Other]),
            loop(Socket, Host, Port)
    end.
