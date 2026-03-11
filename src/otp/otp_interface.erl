-module(otp_interface).

%%% @doc
%%% A minimal, self-contained implementation of the gen_server contract.
%%%
%%% This module provides the scaffolding that `gen_server` gives you for free:
%%% spawning a long-lived process, running a tail-recursive message loop,
%%% dispatching incoming messages to callbacks, and implementing a
%%% reference-based synchronous request/reply protocol.
%%%
%%% Reading this alongside `otp.erl` (which implements the callbacks) and then
%%% comparing the pair with `server.erl` (which uses the real gen_server) is
%%% the fastest way to understand what OTP behaviours actually do.
%%%
%%% ── Message protocol (internal wire format) ──────────────────────────────
%%%
%%%   {sync,  CallerPid, Ref, Msg}
%%%       Synchronous call.  The caller blocks in a receive waiting for a
%%%       message tagged with the same Ref.  Using a unique reference instead
%%%       of the caller's Pid prevents replies from colliding with unrelated
%%%       messages that may already be in the mailbox.
%%%
%%%   {async, Msg}
%%%       Asynchronous cast.  No reply is expected; the caller returns
%%%       immediately.
%%%
%%% ── Callback contract (implemented by the behaviour module, e.g. `otp`) ──
%%%
%%%   Module:init(InitArgs)                       → InitialState
%%%   Module:handle_cast(Msg, State)              → NewState
%%%   Module:handle_call(Msg, {Pid, Ref}, State)  → NewState
%%%       Note: handle_call MUST call answer_call/2 before returning.
%%%
%%% @end

-export([start/2, sync_call/2, async_call/2, answer_call/2]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn a new process that runs Module's callback loop.
%%      InitialState is passed verbatim to Module:init/1.
-spec start(Module :: module(), InitialState :: term()) -> pid().
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

%% @doc Synchronous call — send Msg and block until the process replies.
%%      A unique reference ensures the reply cannot be confused with any
%%      other message already sitting in the caller's mailbox.
%%      Raises `{error, timeout}` if no reply arrives within 1 second.
-spec sync_call(Pid :: pid(), Msg :: term()) -> term().
sync_call(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Resp} -> Resp
    after 1000 ->
        erlang:error(timeout)
    end.

%% @doc Asynchronous cast — deliver Msg without waiting for an acknowledgement.
-spec async_call(Pid :: pid(), Msg :: term()) -> ok.
async_call(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

%% @doc Send the reply back to the caller identified by {Pid, Ref}.
%%      Must be called from inside a handle_call/3 callback — the framework
%%      does not send any implicit reply.
-spec answer_call({pid(), reference()}, Reply :: term()) -> ok.
answer_call({Pid, Ref}, Msg) ->
    Pid ! {Ref, Msg},
    ok.

%%====================================================================
%% Internal
%%====================================================================

%% Bootstrap: initialise the callback module, then enter the receive loop.
-spec init(module(), term()) -> no_return().
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

%% Tail-recursive receive loop — dispatches messages to the right callback
%% and loops with the new state.  Never returns under normal operation.
-spec loop(module(), term()) -> no_return().
loop(Module, State) ->
    receive
        {sync, Pid, Ref, Msg} ->
            %% Synchronous path: the callback is responsible for calling
            %% answer_call/2 to unblock the waiting caller.
            NewState = Module:handle_call(Msg, {Pid, Ref}, State),
            loop(Module, NewState);
        {async, Msg} ->
            %% Asynchronous path: fire-and-forget, no reply needed.
            NewState = Module:handle_cast(Msg, State),
            loop(Module, NewState)
    end.
