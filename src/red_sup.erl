%%%-------------------------------------------------------------------
%% @doc Root supervisor for the `red` application.
%%
%% This is the top of the supervision tree.  It uses `one_for_all`
%% strategy with intensity=0, meaning any single child crash brings
%% down the entire application and lets the OS or an external tool
%% restart it.  That is intentionally strict — networking modules in
%% this repo are started manually from the shell, not as supervised
%% children, so the root supervisor acts purely as a structural anchor
%% for the OTP application framework.
%%
%% To add supervised children, append child specs to `ChildSpecs`
%% following the pattern documented in `src/genServer/sv.erl`.
%%
%% @end
%%%-------------------------------------------------------------------

-module(red_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 0,   % zero tolerance — any crash is fatal for the app
        period    => 1
    },
    ChildSpecs = [],      % networking modules are started manually from the shell
    {ok, {SupFlags, ChildSpecs}}.
