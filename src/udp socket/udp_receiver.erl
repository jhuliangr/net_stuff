-module(udp_receiver).
-export([start/1, stop/0]).

%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
start(Port) ->
    % server will have the clients array
    server:start_link(),
    register(receiver, spawn(fun() ->listen(Port) end)).

stop()->
    receiver ! stop.

%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%

listen(Port) ->
    try
        {ok, Socket} = gen_udp:open(Port,[{active, true}, binary]),
        logger:notice("UDP socket receiving data on port: ~p, socket: ~p~n", [Port, Socket]),
        loop(Socket)
    catch
        Any ->
            logger:error("Error: ~p~n", [Any])
    end.

loop(Socket) ->
    receive
        {udp, Socket, _, _, <<"close">>} ->
            exit_udp(Socket),
            ok;

        {udp, Socket, Host, Port, Datos} ->
            % get all the clients
            Map = server:get_data(),
            logger:notice("Received: ~p from: ~p on port ~p~n", [Datos, Host, Port]),
            % Check if the actual client is in the clients array
            case maps:is_key(Port, Map) of 
                false ->
                    server:store(Port, Socket),
                    NewMap = maps:put(Port, Socket, Map);
                true ->
                    NewMap = Map
            end,
            % send the message to each of the clients 
            maps:foreach(fun(MPort, MSocket) ->
                try
                    gen_udp:send(MSocket, Host, MPort, [<<"Received: ">>, Datos])
                catch
                    {error,closed} ->
                        io:format("Error::: port closed"),
                        store:delete(MPort);
                    Any -> 
                        io:format("Error::: ~p", [Any])
                end 
            end, NewMap),
            loop(Socket);

        {udp_error,_,econnreset} ->
            logger:error("Error connecting ~n", []),
            loop(Socket);

        stop ->
            exit_udp(Socket),
            ok;
        Msg ->
            logger:notice("It was received: ~p, which is not a known command for this receiver. .~n",[Msg]),
            loop(Socket)
    end.

exit_udp(Socket) ->
    logger:notice("Receiver closed"),
    server:stop(),
    gen_udp:close(Socket),
    exit(self(), shutdown).

