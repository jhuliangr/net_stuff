%%%-------------------------------------------------------------------
%% @doc red public API
%% @end
%%%-------------------------------------------------------------------

-module(red_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    red_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
