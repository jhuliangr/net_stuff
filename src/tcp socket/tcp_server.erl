-module(tcp_server).
-export([start/1, stop/0, message/2]).

%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
start(Port) ->
    % Registering the process as db to make it accesible in the whole module
    register(db, otp:start()),
    % Starting the gen_server to save the port
    server:start_link(),
    % Returning a process
    spawn(fun() -> listen(Port) end).

message(Pid, Msg) -> % mensaje desde el server al cliente poner en el PID del server
    Pid ! {msg_for_clients, Msg}.

stop() ->
    % Disconnecting all clients
    {Map} = otp:get_data(db),
    maps:foreach(fun(Port, Socket) ->
        otp:delete(db, Port),
        gen_tcp:close(Socket)
    end, Map),
    % Turning off the OTP scoped database
    unregister(db),
    % Closing the socket to be able to use it again
    gen_tcp:close(server:get(socket)),
    % Turning off the gen_server
    server:stop().

%===========================================================================================================%
%       Private Functions:                                                                                   %
%===========================================================================================================%

listen(Puerto) ->
    {ok, Socket} = gen_tcp:listen(Puerto, [{active, true},{mode, binary}]),
    logger:alert("TCP Server Started on Port: ~p~n", [Puerto]),
    % Storing the socket into the gen_server
    server:store(socket, Socket),
    accepter(Socket).

accepter(SocketListener) ->
    try
        {ok, Socket} = gen_tcp:accept(SocketListener),
        % Each client will have its own process
        spawn(?MODULE, accepter,[SocketListener]),
        % Get client's attributes and save them into the OTP scoped database
        {ok, {Host, Puerto}} = inet:peername(Socket),
        otp:save(db, Puerto, Socket),
        io:format("Client connected: ~p in Port: ~p~n", [Host, Puerto]),
        loop(Socket, Host, Puerto)
    catch
        error:{badmatch,{error,closed}} ->
            io:format("Error, a badmatch ocurred~n")
    end.

loop(Socket, Host, Port) ->
    receive 
        {tcp, Socket, <<"exit", _/binary>>} ->
            logger:notice("It was received a client's petition for closing the socket ~p in the port ~p~n", [Host, Port]),
            otp:delete(db, Port),
            gen_tcp:close(Socket);

        {tcp, Socket, Msg} ->
            logger:alert("It was received ~p by the port ~p the next information: ~p~n", [Host, Port, Msg]),
            % procesing the message
            gen_tcp:send(Socket, [<<"Server received your message, it was: ">>, Msg]),
            loop(Socket, Host, Port);

        {tcp_closed, _Socket} ->
            otp:delete(db, Port),
            logger:alert("Client: ~p diconnected, the port: ~p is free now~n", [Host, Port]);

        {msg_for_clients, Msg} ->
            % receiving the clients list from the OTP scoped database
            {Map} = otp:get_data(db),
            % sending to each of them the message
            maps:foreach(fun(_, Value) ->
                gen_tcp:send(Value, [Msg])
            end, Map),
            loop(Socket, Host, Port);

        {error, closed} ->
            otp:delete(db, Port),
            io:format("There was an error, client ~p was disconnected, port ~p is free", [Host, Port]);

        Other ->
            logger:alert("Unknown command received from: ~p it is: ~p~n", [Host, Other]),
            loop(Socket, Host, Port)
    end.
