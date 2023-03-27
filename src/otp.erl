-module(otp).
-export([start/0, save/3, imprime/1, get/2, clear/1, llaves/1]).
-export([init/1, handle_call/3,handle_cast/2]).

start() ->
    interfazOTP:start(?MODULE, [maps:new()]).
    % spawn(fun()->init(maps:new())end).

init([Estado]) ->
    Estado.

% Funciones...............................
save(Pid, Key, Val) ->
    interfazOTP:async_call(Pid, {store, Key, Val}).
    % Pid ! {store, Key, Val}.

imprime(Pid) -> % imrimir todos los elementos
    interfazOTP:async_call(Pid, {print}).
    % Pid ! {print}.

clear(Pid) -> % vacia el maps
    interfazOTP:async_call(Pid, {clear}).
    % Pid ! {clear}.

get(Pid, Key) -> % obtener un elemento por su clave
    interfazOTP:sync_call(Pid, {get, Key}).
    % Ref = erlang:make_ref(), % da siempre que se llama un id unico
    % Pid ! {self(), Ref, get, Key},
    % receive 
    %     {Ref, Res} -> Res
    %     after 1000 -> 
    %             erlang:error(timeout)
    % end.

llaves(Pid) -> % devuelve el maps
    interfazOTP:sync_call(Pid, {getLlaves}).
    % Ref = erlang:make_ref(),
    % Pid ! {self(), Ref, getLlaves},
    % receive 
    %     {Ref, Res} -> Res
    %     after 1000 -> 
    %         erlang:error(timeout)
    % end.

%===============================================================================
%                               Handlers
%===============================================================================
handle_cast({store, Key, Val}, Json) ->
    maps:put(Key, Val, Json);

handle_cast({print}, Json) ->
    io:format("Json: ~p~n",[Json]),
    Json;

handle_cast({clear}, _Json) ->
    maps:new().

handle_call({ get, Key}, PidRef, Json) ->
    case maps:find(Key, Json) of
        {ok, Val} -> 
            interfazOTP:responder(PidRef, {Val});
        _ ->
            interfazOTP:responder(PidRef, {null})
    end,
    Json;
handle_call({getLlaves}, PidRef, Json) ->
    interfazOTP:responder(PidRef, {maps:keys(Json)}),
    Json;
handle_call(_, _, _) ->
    io:fwrite("Comando Desconocido",[]).




%Proceso.....................................................................
% loop(Json) ->
%     receive
%         {store, Key, Val} ->
%             NewJson = maps:put(Key, Val, Json),
%             io:format("Json actual: ~p~n",[NewJson]),
%             loop(NewJson);

%         {Pid, Ref, {get, Key}} ->
%             case maps:find(Key, Json) of
%                 {ok, Val} -> 
%                     % Pid ! {Ref, Val};
%                     interfazOTP:responder(Pid, Ref, {Val});
%                 _ ->
%                     % Pid ! {Ref, null}
%                     interfazOTP:responder(Pid, Ref, {null})
%             end,
%             loop(Json);

%         {Pid, Ref, {getLlaves}} ->
%             % Pid ! {Ref, maps:keys(Json)},
%             interfazOTP:responder(Pid, Ref, {maps:keys(Json)}),
%             loop(Json);

%         {print} ->
%             io:format("Json: ~p~n",[Json]),
%             loop(Json);

%         {clear} ->
%             loop(maps:new());

%         _ ->
%             io:fwrite("Comando Desconocido",[]),
%             loop(Json)
%     end.

