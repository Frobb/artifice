-module(artifice_sup).

-behaviour(supervisor).

%%% API
-export([start_link/0]).

%%% Supervisor callbacks
-export([init/1]).

%%% API ------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% Supervisor callbacks -------------------------------------------------------

init([]) ->
    ChunkSup = supervisor_child_spec(artifice_chunk_sup),
    CreatureSup = supervisor_child_spec(artifice_creature_sup),
    {ok, { {one_for_one, 5, 10}, [ChunkSup, CreatureSup]} }.

%% @doc Generate a child spec for a supervisor-type child.
%% @private
supervisor_child_spec(Name) ->
    Id = Name,
    StartFunc = {Name, start_link, []},
    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,
    Modules = [Name],
    {Id, StartFunc, Restart, Shutdown, Type, Modules}.
