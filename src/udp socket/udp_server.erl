-module(udp_server).
-export([start/0, listen/1, loop/1]).

-define(PORT, 3000).

start() ->
    spawn(?MODULE, listen, [?PORT]).
    % udp:listen(?PORT).

listen(Puerto) ->
    {ok, Socket} = gen_udp:open(Puerto,[{active, true}, binary]),
    logger:notice("Server Listening in port ~p~n", [Puerto]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Puerto, Datos} ->
            logger:notice("Recibido: ~p desde el host: ~p en el puerto ~p~n", [Datos, Host, Puerto]),
            gen_udp:send(Socket, Host, Puerto, [<<"Recibido: ">>, Datos]),
            loop(Socket);
        stop ->
            ok;
        Mensaje ->
            logger:notice("Se recibio: ~p, lo cual no es un comando para este programa...~n",[Mensaje]),
            loop(Socket)
    end.