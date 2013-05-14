-module(artifice_creature_registry).

-export([register/3, unregister/1, whereis/1, update_pos/2, print/0, init/0]).

%%% API -------------------------------------------------------------------------

%% @doc Register a new creature
register(Cid, Pid, Pos) ->
    ets:insert(?MODULE, {Cid, Pid, Pos}).

%% @doc Unregister a terminated creature
unregister(Cid) ->
    ets:delete(?MODULE, Cid).

%% @doc Look up pid associated with Cid
whereis(Cid) ->
    ets:lookup_element(?MODULE, Cid, 2).

%% @doc Update creature position
update_pos(Cid, NewPos) ->
    ets:update_element(?MODULE, Cid, {3, NewPos}).

%% @doc Print the contents of the registry
print() ->
    ets:tab2list(?MODULE).

%% @doc Start up the creature registry
init() ->
    ets:new(?MODULE, [set, public, named_table, {write_concurrency, true}]).
