-module(otp).

%%% @doc
%%% Map-based key-value store built on the hand-rolled OTP scaffold in
%%% `otp_interface`.
%%%
%%% The public API is intentionally identical to the one in `server.erl`
%%% (which uses the standard `gen_server` behaviour), so the two modules can
%%% be swapped and compared.  The key difference is that this module holds
%%% a Pid explicitly (no name registration), while `server` registers itself
%%% locally and is called by name.
%%%
%%% Conceptually this module only needs to implement three callbacks:
%%%
%%%   init/1         — produce the initial state from constructor arguments
%%%   handle_cast/2  — handle asynchronous (fire-and-forget) messages
%%%   handle_call/3  — handle synchronous (blocking) messages and reply
%%%
%%% The process lifecycle, message routing, and reference-based reply
%%% protocol are handled entirely by `otp_interface`.
%%%
%%% @end

%%  Public API
-export([start/0, save/3, print/1, get/2, clear/1, get_keys/1, get_data/1,
         delete/2, stop/0]).

%%  otp_interface callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn the store process.
%%      Returns a Pid that must be threaded through every subsequent call.
%%      Unlike gen_server there is no implicit local/global name registration.
-spec start() -> pid().
start() ->
    otp_interface:start(?MODULE, [maps:new()]).

%% @doc Asynchronously insert or overwrite Key with Val.
-spec save(Pid :: pid(), Key :: term(), Val :: term()) -> ok.
save(Pid, Key, Val) ->
    otp_interface:async_call(Pid, {store, Key, Val}).

%% @doc Asynchronously pretty-print the entire map to stdout.
-spec print(Pid :: pid()) -> ok.
print(Pid) ->
    otp_interface:async_call(Pid, {print}).

%% @doc Asynchronously erase all entries.
-spec clear(Pid :: pid()) -> ok.
clear(Pid) ->
    otp_interface:async_call(Pid, {clear}).

%% @doc Asynchronously remove the entry for Key.
-spec delete(Pid :: pid(), Key :: term()) -> ok.
delete(Pid, Key) ->
    otp_interface:async_call(Pid, {delete, Key}).

%% @doc Synchronously look up Key.
%%      Returns the stored value, or the atom `null` if the key is absent.
%%      Times out (raises `timeout`) after 1 second.
-spec get(Pid :: pid(), Key :: term()) -> term() | null.
get(Pid, Key) ->
    otp_interface:sync_call(Pid, {get, Key}).

%% @doc Synchronously return the list of all stored keys.
-spec get_keys(Pid :: pid()) -> [term()].
get_keys(Pid) ->
    otp_interface:sync_call(Pid, {get_keys}).

%% @doc Synchronously return the entire map.
-spec get_data(Pid :: pid()) -> map().
get_data(Pid) ->
    otp_interface:sync_call(Pid, {get_data}).

%% @doc Terminate the store process with reason `shutdown`.
-spec stop() -> ok.
stop() ->
    exit(shutdown).

%%====================================================================
%% otp_interface callbacks
%%====================================================================

%% Called once by otp_interface:start/2.
%% We receive the list passed to start/0 and unpack the initial (empty) map.
init([State]) ->
    State.

%% ── Asynchronous handlers ────────────────────────────────────────────────────
%% Unlike gen_server, handle_cast returns the new state directly (no wrapper
%% tuple).  otp_interface uses the return value as the next loop state.

handle_cast({store, Key, Val}, Maps) ->
    maps:put(Key, Val, Maps);

handle_cast({print}, Maps) ->
    io:format("State: ~p~n", [Maps]),
    Maps;

handle_cast({delete, Key}, Maps) ->
    maps:remove(Key, Maps);

handle_cast({clear}, _Maps) ->
    maps:new().

%% ── Synchronous handlers ─────────────────────────────────────────────────────
%% handle_call must call answer_call/2 to unblock the waiting caller, then
%% return the (possibly unchanged) state.

handle_call({get, Key}, PidRef, Maps) ->
    Reply = case maps:find(Key, Maps) of
        {ok, Val} -> {Val};
        error     -> {null}
    end,
    otp_interface:answer_call(PidRef, Reply),
    Maps;

handle_call({get_keys}, PidRef, Maps) ->
    otp_interface:answer_call(PidRef, {maps:keys(Maps)}),
    Maps;

handle_call({get_data}, PidRef, Maps) ->
    otp_interface:answer_call(PidRef, {Maps}),
    Maps;

handle_call(Unknown, _PidRef, Maps) ->
    io:format("[otp] unknown call: ~p~n", [Unknown]),
    Maps.
