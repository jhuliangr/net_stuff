-module(otp).

%           Api Export
-export([start/0, save/3, print/1, get/2, clear/1, get_keys/1, get_data/1, delete/2, stop/0]).

%           Callback export
-export([init/1, handle_call/3,handle_cast/2]).

% Functions..................................................................................................
start() ->
    otp_interface:start(?MODULE, [maps:new()]).
    % spawn(fun()->init(maps:new())end).


save(Pid, Key, Val) ->
    otp_interface:async_call(Pid, {store, Key, Val}).
    % Pid ! {store, Key, Val}.

print(Pid) -> % Print all the elements
    otp_interface:async_call(Pid, {print}).
    % Pid ! {print}.

clear(Pid) -> % To clear the maps content
    otp_interface:async_call(Pid, {clear}).
    % Pid ! {clear}.

delete(Pid, Key) -> % For removing a pair using its key 
    otp_interface:async_call(Pid, {delete, Key}).
    % Pid ! {delete, Key}.

get(Pid, Key) -> % For reading a value using its key to access it
    otp_interface:sync_call(Pid, {get, Key}).
    % Ref = erlang:make_ref(),
    % Pid ! {self(), Ref, get, Key},
    % receive 
    %     {Ref, Res} -> Res
    %     after 1000 -> 
    %             erlang:error(timeout)
    % end.

get_keys(Pid) -> % For retrieving all the keys stored
    otp_interface:sync_call(Pid, {get_keys}).
    % Ref = erlang:make_ref(),
    % Pid ! {self(), Ref, get_keys},
    % receive 
    %     {Ref, Res} -> Res
    %     after 1000 -> 
    %         erlang:error(timeout)
    % end.

get_data(Pid) ->
    otp_interface:sync_call(Pid, {get_data}).

stop() ->
    exit(shutdown).
%===========================================================================================================%
%                               Handlers                                                                    %
%===========================================================================================================%
init([State]) ->
    State.

handle_cast({store, Key, Val}, Maps) ->
    maps:put(Key, Val, Maps);

handle_cast({print}, Maps) ->
    io:format("Maps: ~p~n",[Maps]),
    Maps;
handle_cast({delete, Key}, Maps) ->
    maps:remove(Key, Maps);

handle_cast({clear}, _Maps) ->
    maps:new().

handle_call({get, Key}, PidRef, Maps) ->
    case maps:find(Key, Maps) of
        {ok, Val} -> 
            otp_interface:answer_call(PidRef, {Val});
        _ ->
            otp_interface:answer_call(PidRef, {null})
    end,
    Maps;
handle_call({get_keys}, PidRef, Maps) ->
    otp_interface:answer_call(PidRef, {maps:keys(Maps)}),
    Maps;
handle_call({get_data}, PidRef, Maps) ->
    otp_interface:answer_call(PidRef, {Maps}),
    Maps;
handle_call(_, _, _) ->
    io:fwrite("Unknown Command",[]).

% Process.....................................................................
% loop(Maps) ->
%     receive
%         {store, Key, Val} ->
%             NewMaps = maps:put(Key, Val, Maps),
%             io:format("Actual Maps: ~p~n",[NewMaps]),
%             loop(NewMaps);

%         {Pid, Ref, {get, Key}} ->
%             case maps:find(Key, Maps) of
%                 {ok, Val} -> 
%                     % Pid ! {Ref, Val};
%                     otp_interface:answer_call(Pid, Ref, {Val});
%                 _ ->
%                     % Pid ! {Ref, null}
%                     otp_interface:answer_call(Pid, Ref, {null})
%             end,
%             loop(Maps);

%         {Pid, Ref, {get_keys}} ->
%             % Pid ! {Ref, maps:keys(Maps)},
%             otp_interface:answer_call(Pid, Ref, {maps:keys(Maps)}),
%             loop(Maps);

%         {print} ->
%             io:format("Maps: ~p~n",[Maps]),
%             loop(Maps);

%         {clear} ->
%             loop(maps:new());

%         _ ->
%             io:fwrite("Unknown Command",[]),
%             loop(Maps)
%     end.

