-module(udp_sender).
%           API functions
-export([start/1, send/1, stop/0]).

-define(SERVER_HOST, {127,0,0,1}).
%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
start(ServerPort) ->
    register(sender, spawn(fun() -> listen(ServerPort+1) end)).

send(Msg) ->
    sender ! {send, Msg}.

% closing the socket 
stop() ->
    sender ! stop.

%===========================================================================================================%
%       Private Functions:                                                                                   %
%===========================================================================================================%

listen(Port) ->
    {ok, Socket} = gen_udp:open(Port, [{active, true}, {mode, binary}]),
    io:format("Client listening in port: ~p~n", [Port]),
    loop(Socket, Port).

loop(Socket, Port) ->
    receive
        {send, Msg} ->
            try
                % ....................................................................................................................
                % poner el puerto en una base de datos otp o gen_server
                % Hacer que se guarde en otra de las bases de datos cada cliente 
                % para cuando reciba el receiver, enviar el mensaje a todos
                gen_udp:send(Socket, ?SERVER_HOST, Port-1, Msg)
            catch 
                Any -> 
                    logger:error("Error sending message to receiver, ERROR: ~p~n", [Any])
            end,
            loop(Socket, Port);

        {udp, Socket,Host, ReceiverPort, Datos} ->
            io:format("Received: ~p from host: ~p on port ~p~n", [Datos, Host, ReceiverPort]),
            loop(Socket, Port);

        stop ->
            exit(self(), shutdown),
            gen_udp:close(Socket);

        {udp_error,Socket,econnreset} ->
            logger:error("Connection Error on socket: ~p~n", [Socket]),
            loop(Socket, Port);

        Mensaje ->
            io:format("Received: ~p, which is not a valid command for the client~n", [Mensaje]),
            loop(Socket, Port)
    end.