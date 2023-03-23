-module(tcp_server).
-export([start/0, loop/3, accepter/1, listen/1]).
-define(PORT, 3000).


start() ->
    spawn(?MODULE, listen, [?PORT]).

listen(Puerto) ->
    Opts = [{active, true},{mode, binary}],
    {ok, Socket} = gen_tcp:listen(Puerto, Opts),
    logger:alert("Server tcp iniciado... Puerto: ~p~n", [Puerto]),
    accepter(Socket). 

accepter(SocketListener) ->
    {ok, Socket} = gen_tcp:accept(SocketListener),
    spawn(fun() -> accepter(SocketListener) end),
    {ok, {Host, Puerto}} = inet:peername(Socket),
    loop(Socket, Host, Puerto).

loop(Socket, Host, Puerto) ->
    receive 
        {tcp, Socket, Datos} ->
            logger:alert("Se recibio desde ~p por el puerto ~p la siguiente informacion: ~p~n", [Host, Puerto, Datos]),
            gen_tcp:send(Socket, [<<"Recibido: ">>, Datos]),
            loop(Socket, Host, Puerto);
        {tcp, Socket, <<"exit", _/binary>>} ->
            logger:notice("Se recibio peticion para cerrar la conexion desde ~p por el puerto ~p~n", [Host, Puerto]),
            gen_tcp:close(Socket);
        {tcp_closed, _Socket} ->
            logger:alert("Se cerro la conexion de ~p por el puerto ~p~n", [Host, Puerto]);
        Mensaje ->
            logger:alert("Error, Se recibio lo siguiente: ~p, que no constituye un comando valido~n", [Mensaje]),
            loop(Socket, Host, Puerto)
        end.

