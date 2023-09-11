-module(udp_receiver).
-export([start/1, stop/0]).

%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
start(Port) ->
    % server will save the clients online willing to send data
    server:start_link(),
    register(otp, otp:start()),
    otp:save(otp, receiver_port, Port),
    register(receiver, spawn(fun() ->listen(Port) end)).

stop()->
    otp:stop(),
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
            logger:notice("Receiver closed"),
            otp:stop(),
            exit(self(), shutdown),
            gen_udp:close(Socket);

        {udp, Socket, Host, Port, Datos} ->
            logger:notice("Received: ~p from: ~p on port ~p~n", [Datos, Host, Port]),
            try
                {Map} = server:get_data(),
                io:format("Mapa: ~p~n", [Map]),
                gen_udp:send(Socket, Host, Port, [<<"Received: ">>, Datos])
            catch
                {error,closed} ->
                    io:format("Error::: port closed");
                Any -> 
                    io:format("Error::: ~p", [Any])
            end, 
            loop(Socket);

        {udp_error,_,econnreset} ->
            logger:error("Error connection reseted~n", []),
            loop(Socket);

        stop ->
            exit(self(), shutdown),
            gen_udp:close(Socket),
            ok;
        Msg ->
            logger:notice("It was received: ~p, which is not a known command for this receiver. .~n",[Msg]),
            loop(Socket)
    end.

