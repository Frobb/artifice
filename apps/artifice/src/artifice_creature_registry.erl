-module(artifice_creature_registry).

-export([register/4]).
-export([unregister/1]).
-export([whereis/1]).
-export([update_pos/2]).
-export([pos_of/1]).
-export([brain_of/1]).
-export([decimate/0]).

-export([print/0]).
-export([init/0]).

%%% API -------------------------------------------------------------------------

%% @doc Register a new creature.
register(Cid, Pid, Pos, Brain) ->
    ets:insert(?MODULE, {Cid, Pid, Pos, Brain}).

%% @doc Unregister a terminated creature.
unregister(Cid) ->
    ets:delete(?MODULE, Cid).

%% @doc Look up pid associated with Cid.
whereis(Cid) ->
    ets:lookup_element(?MODULE, Cid, 2).

%% @doc Update creature position.
update_pos(Cid, NewPos) ->
    ets:update_element(?MODULE, Cid, {3, NewPos}).

%% @doc Get the position of the specified creature.
pos_of(Cid) ->
    ets:lookup_element(?MODULE, Cid, 3).

%% @doc Get the brain of the specified creature.
brain_of(Cid) ->
    ets:lookup_element(?MODULE, Cid, 4).

%% @doc Print the contents of the registry.
print() ->
    ets:tab2list(?MODULE).

%% @doc Start up the creature registry.
init() ->
    ets:new(?MODULE, [set, public, named_table, {write_concurrency, true}]).

%% @doc Kill every 10th creature.
decimate() ->
    Creatures = [Pid || {_, Pid, _, _} <- ets:tab2list(?MODULE)],
    Candidates = lists:zip(Creatures, lists:seq(1, length(Creatures))),
    lists:map(fun artifice_creature:kill/1,
              [Pid || {Pid, N} <- Candidates, N rem 10 == 0]).
