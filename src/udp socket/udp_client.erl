-module(udp_client).
%           API functions
-export([start/0, listen/1, loop/1]).

-define(PORT, 3010).
-define(SERVER_PORT, 3000).
-define(SERVER_HOST, {127,0,0,1}).
%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
% Starting as a process
start() ->
    spawn(?MODULE, listen, [?PORT]).
    % udp_client:listen(?PORT).

listen(Port) ->
    {ok, Socket} = gen_udp:open(Port, [{active, true}, {mode, binary}]),
    io:format("Client listening in port: ~p~n", [Port]),
    loop(Socket).

loop(Socket) ->
    receive
        {enviar, Mensaje} ->
            gen_udp:send(Socket, ?SERVER_HOST, ?SERVER_PORT, Mensaje),
            loop(Socket);
        {udp, Socket,Host, Puerto, Datos} ->
            io:format("Recibido: ~p desde el host: ~p en el puerto ~p~n", [binary_to_list(Datos), Host, Puerto]),
            loop(Socket);
        stop ->
            ok;
        Mensaje ->
            io:format("Se recibio: ~p, lo cual no es un comando valido para el cliente~n", [Mensaje]),
            loop(Socket)
    end.