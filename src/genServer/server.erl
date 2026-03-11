-module(server).

%%% @doc
%%% Map-based key-value store implemented with the standard `gen_server`
%%% OTP behaviour.
%%%
%%% This is the reference implementation in this repo.  Compare it with
%%% `otp.erl` / `otp_interface.erl` to understand exactly what the framework
%%% gives you: structured init/terminate lifecycle, typed return tuples,
%%% hot-code upgrade hooks, and transparent process registration.
%%%
%%% ── Registration ─────────────────────────────────────────────────────────
%%%
%%%   The process is registered *locally* under the atom `server`, meaning it
%%%   is reachable only on the current Erlang node.  A commented-out
%%%   `{global, ?SERVER}` variant is included at the bottom of the API section
%%%   — switch to it when you want the store to be visible from any node in a
%%%   distributed cluster (see src/genServer/connect.md).
%%%
%%% ── Post-init timeout ────────────────────────────────────────────────────
%%%
%%%   init/1 returns `{ok, State, 6000}`.  If no message arrives within 6 s
%%%   of startup, the runtime injects a `timeout` atom into handle_info/2.
%%%   This mechanism is useful for detecting idle processes or triggering
%%%   deferred initialisation work.
%%%
%%% @end

-define(SERVER, ?MODULE).

%%  Public API
-export([start_link/0, store/2, get/1, delete/1, get_keys/0, get_data/0,
         clear/0, sleep/0, stop/0]).

%%  gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%%====================================================================
%% API — local node registration
%%====================================================================

%% @doc Start the gen_server and register it locally as `server`.
%%      The type spec enumerates all outcomes gen_server:start_link/4 can
%%      produce so callers can pattern-match exhaustively.
-spec start_link() -> {ok, pid()}
                    | {error, {already_started, pid()}}
                    | {error, term()}
                    | ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Async write — returns immediately; the map update happens inside the
%%      server process on the next iteration of its receive loop.
-spec store(Key :: term(), Val :: term()) -> ok.
store(Key, Val) ->
    gen_server:cast(?SERVER, {store, Key, Val}).

%% @doc Sync read — blocks until the server replies with the stored value.
%%      Returns the atom `null` when the key is not present.
-spec get(Key :: term()) -> term() | null.
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

%% @doc Async delete — removes the entry for Key without blocking the caller.
-spec delete(Key :: term()) -> ok.
delete(Key) ->
    gen_server:cast(?SERVER, {delete, Key}).

%% @doc Sync — return the list of all stored keys.
-spec get_keys() -> [term()].
get_keys() ->
    gen_server:call(?SERVER, {get_keys}).

%% @doc Sync — return the entire map.
-spec get_data() -> map().
get_data() ->
    gen_server:call(?SERVER, {get_data}).

%% @doc Sync sleep — blocks the *server* (and by extension the caller) for 6 s.
%%      Shows why you must pass an explicit timeout when a call can legitimately
%%      take longer than gen_server's default 5 s limit.
-spec sleep() -> ok.
sleep() ->
    gen_server:call(?SERVER, {sleep}, 6001).    % 6001 > the 6000ms sleep inside

%% @doc Async clear — replace the current state with an empty map.
-spec clear() -> ok.
clear() ->
    gen_server:cast(?SERVER, {clear}).

%% @doc Initiate a graceful shutdown.
%%      The {stop, normal, State} in handle_cast/2 triggers terminate/2.
-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, {finish}).

%% ── Global registration — uncomment to share the store across a cluster ──────
%%
%% Replace {local, ?SERVER} with {global, ?SERVER} in start_link/0 and use
%% {global, ?SERVER} as the target in every gen_server:call/cast call below.
%%
%% start_link() -> gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
%% store(Key, Val) -> gen_server:cast({global, ?SERVER}, {store, Key, Val}).
%% get(Key)        -> gen_server:call({global, ?SERVER}, {get, Key}).
%% get_keys()      -> gen_server:call({global, ?SERVER}, {get_keys}).
%% sleep()         -> gen_server:call({global, ?SERVER}, {sleep}, 6001).
%% clear()         -> gen_server:cast({global, ?SERVER}, {clear}).
%% stop()          -> gen_server:cast({global, ?SERVER}, {finish}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, map()}
               | {ok, map(), timeout()}
               | {ok, map(), hibernate}
               | {stop, term()}
               | ignore.
init([]) ->
    %% trap_exit: converts EXIT signals from linked processes into messages,
    %% ensuring terminate/2 is always called for a clean shutdown.
    process_flag(trap_exit, true),
    {ok, maps:new(), 6000}.

%%--------------------------------------------------------------------
%% Synchronous handlers  (gen_server:call)
%%--------------------------------------------------------------------

handle_call({get, Key}, _From, State) ->
    Value = case maps:find(Key, State) of
        {ok, Found} -> Found;
        error       -> null       % atom `null` signals "key not present"
    end,
    {reply, Value, State};

handle_call({get_keys}, _From, State) ->
    {reply, maps:keys(State), State};

handle_call({get_data}, _From, State) ->
    {reply, State, State};

handle_call({sleep}, _From, State) ->
    %% Block the server process for 6 s.  The calling process must use a
    %% timeout > 6 s (hence 6 001 ms in sleep/0) to avoid a gen_server timeout
    %% exception on the caller side.
    timer:sleep(6000),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    io:format("[server] unexpected call: ~p~n", [Req]),
    {reply, {error, unknown_call}, State}.

%%--------------------------------------------------------------------
%% Asynchronous handlers  (gen_server:cast)
%%--------------------------------------------------------------------

handle_cast({store, Key, Val}, State) ->
    {noreply, maps:put(Key, Val, State)};

handle_cast({delete, Key}, State) ->
    {noreply, maps:remove(Key, State)};

handle_cast({clear}, _State) ->
    {noreply, maps:new()};

handle_cast({finish}, State) ->
    %% Returning {stop, Reason, State} causes gen_server to invoke terminate/2
    %% and then exit.  `normal` means expected shutdown, not a crash.
    {stop, normal, State};

handle_cast(Req, State) ->
    io:format("[server] unexpected cast: ~p~n", [Req]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Out-of-band messages  (handle_info)
%%--------------------------------------------------------------------

handle_info(timeout, State) ->
    %% Fired by the 6 000 ms timeout set in init/1 when the mailbox is idle.
    io:format("[server] post-init idle timeout fired~n"),
    {noreply, State};

handle_info(Info, State) ->
    io:format("[server] unexpected message: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Lifecycle
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    io:format("[server] process terminated~n"),
    ok.

%% Called during a hot code upgrade.  Return the state unchanged unless a
%% data-format migration is required.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, State) ->
    State.
