-module(artifice_creature_registry).

-export([register/2, unregister/1, whereis/1, init/0]).

%%% API -------------------------------------------------------------------------

%% @doc Register a new creature
register(Cid, Pid) ->
    ets:insert(?MODULE, {Cid, Pid}).

%% @doc Unregister a terminated creature
unregister(Cid) ->
    ets:delete(?MODULE, Cid).

%% @doc Look up pid associated with Cid
whereis(Cid) ->
    [{Cid, Pid}] = ets:lookup(?MODULE, Cid),
    Pid.

%% @doc Start up the creature registry
init() ->
    ets:new(?MODULE, [set, public, named_table, {read_concurrency, true}]).

