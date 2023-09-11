-module(udp_sender).
%           API functions
-export([start/1, send/1, stop/0]).

-define(SERVER_HOST, {127,0,0,1}).
%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
start(ServerPort) ->
    % look for an open port
    Port = find_port(ServerPort+1),
    register(sender, spawn(fun() -> listen(Port, ServerPort) end)).

send(Msg) ->
    sender ! {send, Msg}.

% closing the socket 
stop() ->
    sender ! stop.

%===========================================================================================================%
%       Private Functions:                                                                                   %
%===========================================================================================================%
find_port(Port) ->
    case gen_udp:open(Port, [binary, {active, false}]) of
        {ok, Socket} ->
            io:format("Port ~p is available.~n", [Port]),
            gen_udp:close(Socket),
            Port;
        {error, _} ->
            io:format("Port ~p is in use, testing next one.~n", [Port]),
            find_port(Port + 1)
    end.

listen(Port, ServerPort) ->
    {ok, Socket} = gen_udp:open(Port, [{active, true}, {mode, binary}]),
    io:format("Client listening in port: ~p~n", [Port]),
    loop(Socket, ServerPort).

loop(Socket, ServerPort) ->
    receive
        {send, Msg} ->
            try
                gen_udp:send(Socket, ?SERVER_HOST, ServerPort, Msg)
            catch 
                Any -> 
                    logger:error("Error sending message to receiver, ERROR: ~p~n", [Any])
            end,
            loop(Socket, ServerPort);

        {udp, Socket,Host, ReceiverPort, Datos} ->
            io:format("Received: ~p from host: ~p on port ~p~n", [Datos, Host, ReceiverPort]),
            loop(Socket, ServerPort);

        stop ->
            try
                gen_udp:send(Socket, ?SERVER_HOST, ServerPort, "out")
            catch 
                Any -> 
                    logger:error("Error sending message to receiver, ERROR: ~p~n", [Any])
            end,
            gen_udp:close(Socket),
            exit(self(), shutdown);

        {udp_error,Socket,econnreset} ->
            logger:error("Connection Error on socket: ~p~n", [Socket]),
            loop(Socket, ServerPort);

        Mensaje ->
            io:format("Received: ~p, which is not a valid command for the client~n", [Mensaje]),
            loop(Socket, ServerPort)
    end.