-module(pub_sub).

%%% @doc
%%% Lightweight publish/subscribe broker backed by a gen_server.
%%%
%%% ── Design decisions ─────────────────────────────────────────────────────
%%%
%%%   Topics are atoms.  The broker state is a Map of Topic => [Pid],
%%%   so lookups and fan-outs are O(subscribers per topic).
%%%
%%%   Dead subscribers are pruned lazily during the next publish to their
%%%   topic, using is_process_alive/1.  This avoids needing monitors in the
%%%   common (no-crash) path while still keeping the registry tidy.
%%%
%%%   The broker registers itself locally as `pub_sub` so callers do not
%%%   need to thread a Pid through their code.
%%%
%%% ── Message contract ─────────────────────────────────────────────────────
%%%
%%%   Every subscriber receives:
%%%     {pub_sub, Topic :: atom(), Payload :: term()}
%%%
%%% ── Usage ────────────────────────────────────────────────────────────────
%%%
%%%   pub_sub:start_link().
%%%   pub_sub:subscribe(news).
%%%   pub_sub:publish(news, "hello subscribers").
%%%   receive {pub_sub, news, Msg} -> io:format("~s~n", [Msg]) end.
%%%   pub_sub:unsubscribe(news).
%%%
%%% @end

-behaviour(gen_server).

%%  Public API
-export([start_link/0, subscribe/1, unsubscribe/1, publish/2]).

%%  gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the broker and register it locally as `pub_sub`.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Subscribe the *calling* process to Topic.
%%      Idempotent — calling twice has no additional effect.
-spec subscribe(Topic :: atom()) -> ok.
subscribe(Topic) ->
    gen_server:cast(?SERVER, {subscribe, Topic, self()}).

%% @doc Remove the calling process from Topic's subscriber list.
-spec unsubscribe(Topic :: atom()) -> ok.
unsubscribe(Topic) ->
    gen_server:cast(?SERVER, {unsubscribe, Topic, self()}).

%% @doc Deliver Payload to every process subscribed to Topic.
%%      Dead subscribers are silently pruned during delivery.
-spec publish(Topic :: atom(), Payload :: term()) -> ok.
publish(Topic, Payload) ->
    gen_server:cast(?SERVER, {publish, Topic, Payload}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% State shape: #{Topic => [Pid]}
init([]) ->
    {ok, #{}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({subscribe, Topic, Pid}, State) ->
    Subs = maps:get(Topic, State, []),
    %% Deduplicate: do not add the same Pid twice.
    NewSubs = case lists:member(Pid, Subs) of
        true  -> Subs;
        false -> [Pid | Subs]
    end,
    {noreply, maps:put(Topic, NewSubs, State)};

handle_cast({unsubscribe, Topic, Pid}, State) ->
    Subs = maps:get(Topic, State, []),
    {noreply, maps:put(Topic, lists:delete(Pid, Subs), State)};

handle_cast({publish, Topic, Payload}, State) ->
    Msg  = {pub_sub, Topic, Payload},
    Subs = maps:get(Topic, State, []),
    %% Deliver and prune dead processes in one pass.
    AliveSubs = lists:filter(
        fun(Pid) ->
            case is_process_alive(Pid) of
                true  -> Pid ! Msg, true;
                false -> false          % drop silently
            end
        end,
        Subs),
    {noreply, maps:put(Topic, AliveSubs, State)};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
