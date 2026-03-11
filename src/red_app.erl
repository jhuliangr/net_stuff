%%%-------------------------------------------------------------------
%% @doc OTP application entry point for the `red` application.
%%
%% The application callback module is the very top of the supervision
%% tree.  Its only job is to start the root supervisor (`red_sup`),
%% which in turn brings up all managed child processes.
%%
%% Start types (`_StartType`):
%%   normal          — standard boot
%%   {takeover, Node} — this node is taking over from Node in a failover
%%   {failover, Node} — Node failed; this node resumes
%%
%% @end
%%%-------------------------------------------------------------------

-module(red_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    red_sup:start_link().

stop(_State) ->
    ok.
