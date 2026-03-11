-module(udp_receiver).

%%% @doc
%%% UDP fan-out hub — every datagram received is re-broadcast to all
%%% currently registered senders.
%%%
%%% ── Client registry ──────────────────────────────────────────────────────
%%%
%%%   Senders register implicitly: the first datagram from a new source
%%%   port is enough for the receiver to add it to the registry (stored in
%%%   the `server` gen_server as Port → Socket).  No explicit handshake is
%%%   required.
%%%
%%%   A sender deregisters by sending the binary `"out"`.  The receiver
%%%   then removes it from the registry and continues.
%%%
%%% ── Special control datagrams ────────────────────────────────────────────
%%%
%%%   "close"   shut down the receiver, gen_server, and the UDP socket
%%%   "out"     remove the sending port from the client registry
%%%
%%% ── Message delivery ─────────────────────────────────────────────────────
%%%
%%%   For each ordinary datagram the receiver calls gen_udp:send/4 once per
%%%   registered client.  Dead or closed sockets are caught and logged; the
%%%   rest of the fan-out continues.
%%%
%%% @end

-export([start/1, stop/0]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the gen_server client registry and the UDP receiver loop.
%%      The receiver process is registered as `receiver` so stop/0 can
%%      reach it without holding a Pid.
-spec start(Port :: inet:port_number()) -> true.
start(Port) ->
    server:start_link(),
    register(receiver, spawn(fun() -> listen(Port) end)).

%% @doc Send a stop signal to the receiver process.
-spec stop() -> ok.
stop() ->
    receiver ! stop,
    ok.

%%====================================================================
%% Private
%%====================================================================

%% @private Open the UDP socket and enter the receive loop.
listen(Port) ->
    try
        {ok, Socket} = gen_udp:open(Port, [{active, true}, binary]),
        logger:notice("UDP receiver listening on port ~p~n", [Port]),
        loop(Socket)
    catch
        Any ->
            logger:error("[udp_receiver] failed to open socket: ~p~n", [Any])
    end.

%% @private Fan-out loop.  In `{active, true}` mode gen_udp delivers each
%%           datagram as a {udp, Socket, Host, Port, Data} message.
loop(Socket) ->
    receive
        %% Control: shut down the receiver entirely.
        {udp, Socket, _, _, <<"close">>} ->
            exit_udp(Socket),
            ok;

        %% Control: remove a sender from the registry (clean deregistration).
        {udp, _, _, Port, <<"out">>} ->
            server:delete(Port),
            loop(Socket);

        %% Ordinary data datagram: register new senders and fan out.
        {udp, Socket, Host, Port, Data} ->
            Map = server:get_data(),
            logger:notice("[udp_receiver] received ~p from ~p:~p~n", [Data, Host, Port]),
            %% Auto-register the sender on first contact.
            NewMap = case maps:is_key(Port, Map) of
                false ->
                    server:store(Port, Socket),
                    maps:put(Port, Socket, Map);
                true ->
                    Map
            end,
            %% Fan out to every registered sender.
            maps:foreach(
                fun(MPort, MSocket) ->
                    try
                        gen_udp:send(MSocket, Host, MPort, [<<"Received: ">>, Data])
                    catch
                        {error, closed} ->
                            %% Dead socket — remove it so it doesn't linger.
                            io:format("[udp_receiver] socket on port ~p closed, removing~n", [MPort]),
                            server:delete(MPort);
                        Err ->
                            io:format("[udp_receiver] send error: ~p~n", [Err])
                    end
                end,
                NewMap),
            loop(Socket);

        %% ICMP port-unreachable translated by the OS into an Erlang error.
        {udp_error, _, econnreset} ->
            logger:error("[udp_receiver] connection reset by peer~n"),
            loop(Socket);

        %% Programmatic shutdown from stop/0.
        stop ->
            exit_udp(Socket),
            ok;

        Msg ->
            logger:notice("[udp_receiver] unknown message: ~p~n", [Msg]),
            loop(Socket)
    end.

%% @private Tear down the server, close the socket, and exit the process.
exit_udp(Socket) ->
    logger:notice("[udp_receiver] shutting down~n"),
    server:stop(),
    gen_udp:close(Socket),
    exit(self(), shutdown).
