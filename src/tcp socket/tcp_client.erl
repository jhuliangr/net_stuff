-module(tcp_client).

%%% @doc
%%% TCP client that connects to a server on localhost and runs its receive
%%% loop in a dedicated spawned process.
%%%
%%% The caller interacts with the client via message-passing:
%%%   send/2          — deliver a message to the server
%%%   close_socket/1  — close the connection and exit the process
%%%
%%% In `{active, true}` mode gen_tcp delivers incoming data as Erlang
%%% messages ({tcp, Socket, Data}), so no explicit recv call is needed.
%%%
%%% @end

%%  API
-export([start/1, send/2, close_socket/1]).

%%  Hardcoded to localhost for now; change to an IP tuple or hostname
%%  when testing across machines.
-define(SERVER_HOST, {127, 0, 0, 1}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn a client process and connect it to ServerPort.
%%      Returns the Pid of the client loop; pass it to send/2 and
%%      close_socket/1.
-spec start(ServerPort :: inet:port_number()) -> pid().
start(ServerPort) ->
    spawn(fun() -> connect(?SERVER_HOST, ServerPort) end).

%% @doc Send Message to the server.  The client process must be the Pid
%%      returned by start/1.
-spec send(Pid :: pid(), Message :: iodata()) -> ok.
send(Pid, Message) ->
    Pid ! {send, Message},
    ok.

%% @doc Close the TCP socket and terminate the client process.
-spec close_socket(Pid :: pid()) -> ok.
close_socket(Pid) ->
    Pid ! {stop},
    ok.

%%====================================================================
%% Private
%%====================================================================

%% @private Attempt to connect to Host:Port.  Errors are reported to
%%           stdout and the process exits; no retry logic is applied.
connect(Host, Port) ->
    try
        {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}]),
        io:format("[tcp_client] connected to ~p:~p~n", [Host, Port]),
        loop(Socket, Host, Port)
    catch
        error:{badmatch, {error, econnrefused}} ->
            io:format("[tcp_client] connection refused (is the server running?)~n");
        error:{badmatch, {error, etimedout}} ->
            io:format("[tcp_client] connection timed out~n");
        error:{badmatch, {error, einval}} ->
            io:format("[tcp_client] invalid host or port argument~n")
    end.

%% @private Receive loop.  gen_tcp delivers data in active mode as
%%           {tcp, Socket, Binary} messages, so we never block on a recv.
loop(Socket, Host, Port) ->
    receive
        %% Data received from the server.
        {tcp, Socket, Data} ->
            io:format("[tcp_client] from ~p:~p -> ~s~n", [Host, Port, binary_to_list(Data)]),
            loop(Socket, Host, Port);

        %% Outgoing message requested by the caller via send/2.
        {send, Message} ->
            gen_tcp:send(Socket, Message),
            loop(Socket, Host, Port);

        %% Server closed the connection.
        {tcp_closed, _} ->
            io:format("[tcp_client] connection closed by server~n");

        %% Caller requested an explicit disconnect.
        {stop} ->
            gen_tcp:close(Socket),
            ok;

        Other ->
            io:format("[tcp_client] unexpected message: ~p~n", [Other]),
            loop(Socket, Host, Port)
    end.
