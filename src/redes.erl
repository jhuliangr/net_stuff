-module(redes).
-export([crearSocketUDP/0]).

crearSocketUDP() ->
    {ok, Socket} = gen_udp:open(1000, []),
    logger:alert("~p~n",[inet:getstat(Socket)]),
    logger:alert("~p~n",[inet:getopts(Socket, [active])]),
    inet:setopts(Socket, [{active, false}]),
    logger:alert("~p~n",[inet:getopts(Socket, [active])]),
    gen_udp:close(Socket).

