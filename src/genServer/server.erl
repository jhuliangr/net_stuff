-module(server).
-define(SERVER, ?MODULE).


%                        export de la API
-export([start_link/0, store/2, get/1, llaves/0, clear/0, sleep/0, stop/0]).
%                        export para CALLBACKS
-export([init/1, handle_call/3, handle_cast/2,handle_info/2, terminate/2, code_change/3, format_status/2]).
%=======================================================================%
%                               API                                     %
%=======================================================================%

% -spec start_link() ->{ok, Pid ::pid()} |{error, Error :: {already_started, pid()}} | { error, Error :: term()}| ignore.
%el spec indica que va a esperar que devlueva la funcion 

% start_link() ->
%     gen_server:start_link(?MODULE, [], []).
% store(Pid, Key, Val) ->
%     gen_server:cast(Pid, {store, Key, Val}).
% get(Pid, Key) ->
%     gen_server:call(Pid, {get, Key}).
% llaves(Pid) ->
%     gen_server:call(Pid, {llaves}).
% clear(Pid) ->
%     gen_server:cast(Pid, {clear}).

% para visibilidad de los datos en el nodo local solamente___________________________________________________________
start_link() -> % al poner {local, ?MODULE}
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
store(Key, Val) ->
    gen_server:cast(?SERVER,{store, Key, Val}).
get(Key) ->
    gen_server:call(?SERVER,{get, Key}).
llaves() ->
    gen_server:call(?SERVER,{llaves}).
clear() ->
    gen_server:cast(?SERVER,{clear}).
sleep() ->
    gen_server:call(?SERVER, {sleep},6001).% el ultimo numero es el limite del timeout
stop() ->
    gen_server:cast(?SERVER, {detener}).

% para visibilidad de los datos desde otros nodos:____________________________________________________________________
% start_link() -> % al poner {global, ?MODULE} 
%     gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
% store(Key, Val) ->
%     gen_server:cast({global, ?SERVER},{store, Key, Val}).
% get(Key) ->
%     gen_server:call({global, ?SERVER},{get, Key}).
% llaves() ->
%     gen_server:call({global, ?SERVER},{llaves}).
% clear() ->
%     gen_server:cast({global, ?SERVER},{clear}).

%=======================================================================%
%                      gen_server CALLBACKS                             %
%=======================================================================%
% -spec init() -> (Args ::term())-> {ok, State :: term()} | {ok, State::term, Timeout::timeout()} | {ok, State::term(), hibernate} | {stop, Reason::term()}|ignore.

init([]) ->
    process_flag(trap_exit, true),
    {ok, maps:new(), 6000}. % 6000 es un timeout

% Handle Calls............................................
handle_call({get, Key}, _From, State) ->
    Value = case maps:find(Key, State) of
        {ok, Found} ->Found;
        _ -> null
    end,
    {reply, Value, State};

handle_call({llaves}, _From, State) ->
    {reply, maps:keys(State), State};

handle_call({sleep}, _From, State) ->
    timer:sleep(6000),
    {reply, ok , State};

handle_call(Req, _, State) ->
    io:format("Mensaje no Esperado TIPO CALL: ~p~n",[Req]),
    {reply, ok, State}.
% Handle Casts.............................................
handle_cast({store, Key, Val}, State) ->
    NewState = maps:put(Key, Val, State),
    {noreply, NewState};

handle_cast({clear}, _State) ->    
    {noreply, maps:new()};

handle_cast({detener}, State) ->    
    {stop, normal, State};

handle_cast(Req, State) ->
    io:format("Mensaje no Esperado TIPO CAST: ~p~n",[Req]),
    {noreply, State}.

% Handle Info .............................................
handle_info(timeout, State) ->
    io:format("Timeout Recibido desde init~n",[]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Handle info no esperado: ~p~n",[Info]),
    {noreply, State}.
% Otras ...................................................
terminate(_, _) ->
    io:format("Proeso terminado cone exito :)", []),
    ok.
code_change(_, State, _) ->
    {ok, State}.
format_status(_, State) ->
    State.

