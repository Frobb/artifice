-module(artifice_chunk).
-behaviour(gen_server).

%%% API
-export([start_link/1]).
-export([ensure_started/1]).

-export([add_creature/3]).
-export([move_creature/3]).
-export([remove_creature/2]).

-export([publish/2]).
-export([subscribe/1]).
-export([unsubscribe/1]).

-export([gridref_of/2]).
-export([adjacent_gridrefs/1]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Represents an event subscriber
-record(sub, {
          pid :: pid(),
          ref :: reference()
         }).

%% Represents a creature
-record(creature, {
          cid :: binary(),
          pos :: {integer(), integer()}
         }).

-record(state, {
          subs :: list(),
          creatures :: list(),
          gridref
         }).

-define(CHUNK_WIDTH, 128).
-define(CHUNK_HEIGHT, 128).

%%% API ------------------------------------------------------------------------

%% @doc Start a chunk server for the given grid reference.
start_link(GridRef) ->
    Name = registered_name(GridRef),
    gen_server:start_link({local, Name}, ?MODULE, [GridRef], []).

%% Starts the chunk server for the given grid ref, if necessary.
ensure_started(GridRef) ->
    case is_started(GridRef) of
        true ->
            ok;
        false ->
            {ok, _Pid} = artifice_chunk_sup:start_child(child_spec(GridRef)),
            ok
    end.

%% @doc Generate a child spec for starting a chunk server from a supervisor.
%% @private
child_spec(GridRef) ->
    Name = registered_name(GridRef),
    StartFunc = {?MODULE, start_link, [GridRef]},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [?MODULE],
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

%% @doc Get the name with which the chunk server for a given grid ref registers.
%% @private
registered_name({X, Y}) ->
    list_to_atom(
      lists:flatten(
        io_lib:format("artifice_chunk_~w_~w", [X, Y])
       )
     ).

%% @doc True if the chunk server handling the given grid ref is alive.
%% @private
is_started(GridRef) ->
    Name = registered_name(GridRef),
    case catch sys:get_status(Name) of
        {status, _Pid, _Mod, _Status} ->
            true;
        {'EXIT', {noproc, _}} ->
            false
    end.

%% @doc Publish an event to all subscribers.
publish(GridRef, Event) ->
    ok = ensure_started(GridRef),
    gen_server:cast(registered_name(GridRef), {publish, Event}).

%% @doc Subscribe the current process to events from a chunk.
subscribe(GridRef) ->
    ok = ensure_started(GridRef),
    gen_server:cast(registered_name(GridRef), {subscribe, self()}).

%% @doc Unsubscribe the current process from events from a chunk.
unsubscribe(GridRef) ->
    ok = ensure_started(GridRef),
    gen_server:cast(registered_name(GridRef), {unsubscribe, self()}).

%% @doc Get the chunk grid reference for a coordinate pair (x,y).
gridref_of(X, Y) ->
    {X div ?CHUNK_WIDTH, Y div ?CHUNK_HEIGHT}.

%% @doc Get a list of adjacent grid references.
adjacent_gridrefs({X, Y}) ->
    [{X, Y},      % Center
     {X, Y+1},    % N
     {X-1, Y+1},  % NW
     {X-1, Y},    % W
     {X-1, Y-1},  % SW
     {X, Y-1},    % S
     {X+1, Y-1},  % SE
     {X+1, Y},    % E
     {X+1, Y+1}]. % NE

%% @doc Add a creature to the given chunk.
add_creature(GridRef, Cid, Pos) ->
    ok = ensure_started(GridRef),
    Name = registered_name(GridRef),
    gen_server:cast(Name, {add_creature, Cid, Pos}).

%% @doc Move a creature within the given chunk.
move_creature(GridRef, Cid, Pos) ->
    ok = ensure_started(GridRef),
    Name = registered_name(GridRef),
    gen_server:cast(Name, {move_creature, Cid, Pos}).

%% @doc Remove a creature from the given chunk.
remove_creature(GridRef, Cid) ->
    ok = ensure_started(GridRef),
    Name = registered_name(GridRef),
    gen_server:cast(Name, {remove_creature, Cid}).

%%% gen_server callbacks -------------------------------------------------------

init([{X,Y}=GridRef]) ->
    lager:info("Chunk (~w,~w) online.", [X, Y]),
    State = #state{gridref=GridRef},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({subscribe, Pid}, #state{gridref=GridRef, subs=Subs0}=State) ->
    lager:debug("Process ~p subscribed to chunk ~p.", [Pid, GridRef]),
    Ref = erlang:monitor(process, Pid),
    Sub = {Pid, #sub{pid=Pid, ref=Ref}},
    Subs1 = [Sub|Subs0],
    {noreply, State#state{subs=Subs1}};
handle_cast({unsubscribe, Pid}, #state{gridref=GridRef, subs=Subs0}=State) ->
    lager:debug("Process ~p unsubscribed from chunk ~p.", [Pid, GridRef]),
    {Pid, #sub{ref=Ref}} = lists:keyfind(Pid, 1, Subs0),
    erlang:demonitor(Ref),
    Subs1 = lists:keydelete(Pid, 1, Subs0),
    {noreply, State#state{subs=Subs1}};
handle_cast({publish, Event}, #state{subs=Subs}=State) ->
    lists:foreach(
      fun({Pid, _}) ->
              Pid ! {event, Event}
      end,
      Subs),
    {noreply, State};
handle_cast({add_creature, Cid, Pos}, #state{creatures=Creatures}=State) ->
    Creature = #creature{cid=Cid, pos=Pos},
    {noreply, State#state{creatures=[{Cid, Creature}|Creatures]}};
handle_cast({move_creature, Cid, Pos}, #state{creatures=Creatures0}=State) ->
    {_, Creature0} = lists:keyfind(Cid, 1, Creatures0),
    Creature1 = Creature0#creature{pos=Pos},
    Creatures1 = lists:keyreplace(Cid, 1, Creatures0, {Cid, Creature1}),
    {noreply, State#state{creatures=Creatures1}};
handle_cast({remove_creature, Cid}, #state{creatures=Creatures0}=State) ->
    Creatures1 = lists:keydelete(Cid, 1, Creatures0),
    {noreply, State#state{creatures=Creatures1}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
