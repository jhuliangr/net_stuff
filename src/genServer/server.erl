-module(server).
-define(SERVER, ?MODULE).


%                       API export 
-export([start_link/0, store/2, get/1, delete/1, get_keys/0, clear/0, sleep/0, stop/0]).

%                       CALLBACKS export 
-export([init/1, handle_call/3, handle_cast/2,handle_info/2, terminate/2, code_change/3, format_status/2]).
%===========================================================================================================%
%                               API                                                                         %
%===========================================================================================================%

% setting datatypes for start_link
-spec start_link() ->{ok, Pid ::pid()} 
                   | {error, Error :: {already_started, pid()}} 
                   | { error, Error :: term()}| ignore.

% For making data visible only for the local node: {local, ?MODULE}__________________________________________

start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
store(Key, Val) ->
    gen_server:cast(?SERVER,{store, Key, Val}).
get(Key) ->
    gen_server:call(?SERVER,{get, Key}).
delete(Key) ->
    gen_server:cast(?SERVER,{delete, Key}).
get_keys() ->
    gen_server:call(?SERVER,{get_keys}).
sleep() ->
    gen_server:call(?SERVER, {sleep},6001).% The Last parameter is a timeout
clear() ->
    gen_server:cast(?SERVER,{clear}).
stop() ->
    gen_server:cast(?SERVER, {finish}).

% For making data visible for all the nodes connected: {global, ?MODULE}_____________________________________

% start_link() -> 
%     gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
% store(Key, Val) ->
%     gen_server:cast({global, ?SERVER},{store, Key, Val}).
% get(Key) ->
%     gen_server:call({global, ?SERVER},{get, Key}).
% get_keys() ->
%     gen_server:call({global, ?SERVER},{get_keys}).
% sleep() ->
%     gen_server:call({global, ?SERVER}, {sleep},6001).
% clear() ->
%     gen_server:cast({global, ?SERVER},{clear}).
% stop() ->
%     gen_server:cast({global, ?SERVER}, {finish}).

%===========================================================================================================%
%                      gen_server CALLBACKS                                                                 %
%===========================================================================================================%
-spec init([]) -> {ok, State :: term()} 
                | {ok, State::term, Timeout::timeout()} 
                | {ok, State::term(), hibernate} 
                | {stop, Reason::term()}|ignore.

init([]) ->
    process_flag(trap_exit, true),
    {ok, maps:new(), 6000}. % 6000 is a timeout

% Handle Calls...............................................................................................
handle_call({get, Key}, _From, State) ->
    Value = case maps:find(Key, State) of
        {ok, Found} ->
            Found;
        _ -> 
            null
    end,
    {reply, Value, State};


handle_call({get_keys}, _From, State) ->
    {reply, maps:keys(State), State};

handle_call({sleep}, _From, State) ->
    timer:sleep(6000),
    {reply, ok , State};

handle_call(Req, _, State) ->
    io:format("Unespected CALL message: ~p~n",[Req]),
    {reply, ok, State}.

% Handle Casts...............................................................................................
handle_cast({store, Key, Val}, State) ->
    NewState = maps:put(Key, Val, State),
    {noreply, NewState};

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState};

handle_cast({clear}, _State) ->    
    {noreply, maps:new()};

handle_cast({finish}, State) ->    
    {stop, normal, State};

handle_cast(Req, State) ->
    io:format("Unespected CAST message: ~p~n",[Req]),
    {noreply, State}.

% Handle Info ...............................................................................................
handle_info(timeout, State) ->
    io:format("Timeout received from init()~n",[]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Unespected Handle Info message: ~p~n",[Info]),
    {noreply, State}.
% Others .....................................................................................................
terminate(_, _) ->
    io:format("Process finished succesfully :) ~n", []),
    ok.
code_change(_, State, _) ->
    {ok, State}.
format_status(_, State) ->
    State.

