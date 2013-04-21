-module(artifice_chunk_sup).
-behaviour(supervisor).

%%% API
-export([start_link/0]).
-export([start_child/1]).

%%% Supervisor callbacks
-export([init/1]).

%%% API ------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

%%% Supervisor callbacks -------------------------------------------------------

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.
