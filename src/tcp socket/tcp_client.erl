-module(tcp_client).
%           API functions
-export([start/1, send/2, close_socket/1]).
%           For now we will work with our localhost
-define(SERVER_HOST, {127,0,0,1}).

%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%

start(ServerPort) ->
    spawn(fun() -> connect(?SERVER_HOST, ServerPort) end).

send(Pid, Message) -> % Message from client to server, Its argument most be the Pid of the client
    Pid ! {send, Message}.

close_socket(Pid) ->
    Pid ! {stop}.

%===========================================================================================================%
%       Private Functions:                                                                                   %
%===========================================================================================================%

connect(Host, Port) ->
    try
        {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}]),
        io:format("Connected to the server ~p successfully on port ~p~n", [Host, Port]),
        loop(Socket, Host, Port)
    catch
        error:{badmatch,{error,econnrefused}} ->
            io:format("Error, connection refused~n");
        error:{badmatch,{error,etimedout}} ->
            io:format("Error, connection timeout~n");
        error:{badmatch,{error,einval}} ->
            io:format("Error, invalid argument~n")
    end.

loop(Socket, Host, Port) ->
    receive
        {tcp, Socket, Datos} ->
            io:format("Received from ~p by port: ~p -> ~p~n", [Host, Port, binary_to_list(Datos)]),
            loop(Socket, Host, Port);
        {send, Message} ->
            gen_tcp:send(Socket, Message),
            loop(Socket, Host, Port);
        {tcp_closed, _} ->
            io:format("TCP connection closed by server~n");
        {stop} ->
            gen_tcp:close(Socket),
            ok;
        Message ->
            io:format("Error, it was received this: ~p~n", [Message]),
            loop(Socket, Host, Port)
        end.

