-module(otp_interface).
-export([start/2, sync_call/2, async_call/2, answer_call/2]).
%===========================================================================================================%
%       Public Functions:                                                                                   %
%===========================================================================================================%
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

sync_call(Pid, Msg) -> 
    Ref = erlang:make_ref(),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Resp} -> Resp
        after 1000 ->
            erlang:error(timeout)
    end.

async_call(Pid, Msg) ->
   Pid ! {async, Msg}.

answer_call({Pid, Ref},  Msg) ->
    Pid ! {Ref, Msg}.

%===========================================================================================================%
%       Private Functions:                                                                                  %
%===========================================================================================================%
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {sync, Pid, Ref, Msg} ->
            NewState = Module:handle_call(Msg, {Pid, Ref}, State),
            loop(Module, NewState);
        {async, Msg} ->
            NewState = Module:handle_cast(Msg, State),
            loop(Module, NewState)
    end.
