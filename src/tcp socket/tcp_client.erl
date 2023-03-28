-module(tcp_client).
-export([start/0, connect/2, loop/3, enviar/2]).


-define(SERVER_PORT, 3000).
-define(SERVER_HOST, {127,0,0,1}).

start() ->
    spawn(?MODULE, connect, [?SERVER_HOST, ?SERVER_PORT]).

connect(Host, Puerto) ->
    Opts = [{active, true}, {mode, binary}],
    {ok, Socket} = gen_tcp:connect(Host, Puerto, Opts),
    io:format("Conectado al server ~p de forma exitosa en el puerto ~p~n", [Host, Puerto]),
    loop(Socket, Host, Puerto).

loop(Socket, Host, Puerto) ->
    receive
        {tcp, Socket, Datos} ->
            io:format("Recibido desde el host ~p por el puerto ~p, lo siguiente: ~p~n", [Host, Puerto, binary_to_list(Datos)]),
            loop(Socket, Host, Puerto);
        {enviar, Mensaje} ->
            gen_tcp:send(Socket, Mensaje),
            loop(Socket, Host, Puerto);
        stop ->
            gen_tcp:close(Socket),
            ok;
        Mensaje ->
            io:format("Error, se recibio: ~p~n", [Mensaje]),
            loop(Socket, Host, Puerto)
        end.

enviar(Pid, Mensaje) -> % mensaje de cliente a server poner en el PID de cliente
    Pid ! {enviar, Mensaje}.



