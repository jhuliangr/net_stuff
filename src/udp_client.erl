-module(udp_client).
-export([start/0, listen/1, loop/1]).
% -compile(export_all).
-define(PORT, 3010).
-define(SERVER_PORT, 3000).
-define(SERVER_HOST, {127,0,0,1}).

start() ->
    spawn(?MODULE, listen, [?PORT]).
    % udp_client:listen(?PORT).

listen(Puerto) ->
    Opts = [{active, true}, {mode, binary}],
    {ok, Socket} = gen_udp:open(Puerto, Opts),
    io:format("Cliente Escuchando en el puerto: ~p~n", [Puerto]),
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