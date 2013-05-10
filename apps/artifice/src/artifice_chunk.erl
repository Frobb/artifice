-module(artifice_chunk).
-behaviour(gen_server).

-include("common.hrl").
-include("event.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% API
-export([start_link/1]).
-export([ensure_started/1]).

-export([add_creature/3]).
-export([move_creature/3]).
-export([remove_creature/2]).
-export([creature_at/2]).

-export([publish/2]).
-export([subscribe/1]).
-export([unsubscribe/1]).
-export([subscribe_initial/1]).
-export([update_subscriptions/2]).
-export([unsubscribe_final/1]).

-export([chunk_at/1]).
-export([adjacent_chunks/1]).
-export([event_log/1]).

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

-record(state, {
          subs = [] :: list(),
          creatures = [] :: list(),
	  log = [] :: list(), 
          chunk
         }).

-define(CHUNK_WIDTH, 128).
-define(CHUNK_HEIGHT, 128).

%%% API ------------------------------------------------------------------------

%% @doc Start a chunk server for the given chunk reference.
start_link(Chunk) ->
    Name = registered_name(Chunk),
    gen_server:start_link({local, Name}, ?MODULE, [Chunk], []).

%% Starts the chunk server for the given chunk ref, if necessary.
ensure_started(Chunk) ->
    Name = registered_name(Chunk),
    case lists:member(Name, registered()) of
        true ->
            ok;
        false ->
            case artifice_chunk_sup:start_child(child_spec(Chunk)) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok
            end
    end.

%% @doc Generate a child spec for starting a chunk server from a supervisor.
%% @private
child_spec(Chunk) ->
    Name = registered_name(Chunk),
    StartFunc = {?MODULE, start_link, [Chunk]},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [?MODULE],
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

%% @doc Get the name with which the chunk server for a given chunk ref registers.
%% @private
registered_name({X, Y}) ->
    list_to_atom(
      lists:flatten(
        io_lib:format("artifice_chunk_~w_~w", [X, Y])
       )
     ).

%% @doc Publish an event to all subscribers.
publish(Chunk, Event) ->
    ok = ensure_started(Chunk),
    gen_server:cast(registered_name(Chunk), {publish, Event}).

%% @doc Subscribe the current process to events from a chunk.
subscribe(Chunk) ->
    ok = ensure_started(Chunk),
    gen_server:cast(registered_name(Chunk), {subscribe, self()}).

%% @doc Unsubscribe the current process from events from a chunk.
unsubscribe(Chunk) ->
    ok = ensure_started(Chunk),
    gen_server:cast(registered_name(Chunk), {unsubscribe, self()}).

%% @doc Update subscriptions for the calling process based
%% on the old and new coordinates. Should be called after
%% moving yourself (creatures) or the camera (clients).
update_subscriptions(OldPos, NewPos) ->
    OldAdj = adjacent_chunks(chunk_at(OldPos)),
    NewAdj = adjacent_chunks(chunk_at(NewPos)),
    lists:foreach(fun artifice_chunk:subscribe/1,   NewAdj -- OldAdj),
    lists:foreach(fun artifice_chunk:unsubscribe/1, OldAdj -- NewAdj).

%% @doc Set up initial chunk subscriptions for the current process.
%% Should be called by a creature or client upon startup.
subscribe_initial(Pos) ->
    lists:foreach(fun subscribe/1, adjacent_chunks(chunk_at(Pos))).

%% @doc Unsubscribes the current process from all chunks in its vicinity.
%% Should be called when a creature or client exits.
unsubscribe_final(Pos) ->
    lists:foreach(fun unsubscribe/1, adjacent_chunks(chunk_at(Pos))).

%% @doc Get the chunk reference for a coordinate pair (x,y).
chunk_at({X,Y}) ->
    {floor(X / ?CHUNK_WIDTH), floor(Y / ?CHUNK_HEIGHT)}.

%% @doc Get a list of adjacent chunk references.
adjacent_chunks({X, Y}) ->
    [{X, Y},      % Center
     {X, Y-1},    % N
     {X-1, Y-1},  % NW
     {X-1, Y},    % W
     {X-1, Y+1},  % SW
     {X, Y+1},    % S
     {X+1, Y+1},  % SE
     {X+1, Y},    % E
     {X+1, Y-1}]. % NE

%% @doc Add a creature to the given chunk.
add_creature(Chunk, Cid, Pos) ->
    ok = ensure_started(Chunk),
    Name = registered_name(Chunk),
    gen_server:cast(Name, {add_creature, Cid, Pos}).

%% @doc Move a creature within the given chunk.
move_creature(Chunk, Cid, Pos) ->
    ok = ensure_started(Chunk),
    Name = registered_name(Chunk),
    gen_server:cast(Name, {move_creature, Cid, Pos}).

%% @doc Remove a creature from the given chunk.
remove_creature(Chunk, Cid) ->
    ok = ensure_started(Chunk),
    Name = registered_name(Chunk),
    gen_server:cast(Name, {remove_creature, Cid}).

%% @doc Get the id of the creature at the given position, or false.
creature_at(Chunk, Pos) ->
    ok = ensure_started(Chunk),
    Name = registered_name(Chunk),
    gen_server:call(Name, {creature_at, Pos}).

event_log(Chunk) ->
    ok = ensure_started(Chunk),
    Name = registered_name(Chunk),
    gen_server:call(Name, event_log).

%%% gen_server callbacks -------------------------------------------------------

init([{X,Y}=Chunk]) ->
    lager:info("Chunk (~w,~w) online.", [X, Y]),
    State = #state{chunk=Chunk},
    {ok, State}.

handle_call({creature_at, Pos}, _From, #state{creatures=Creatures}=State) ->
    {reply, find_creature_by_pos(Creatures, Pos), State};

handle_call(event_log, _From, #state{log=Log}=State) ->
    {reply, lists:reverse(Log), State}.

handle_cast({subscribe, Pid}, #state{chunk=Chunk, subs=Subs0}=State) ->
    lager:debug("Process ~p subscribed to chunk ~p.", [Pid, Chunk]),
    Ref = erlang:monitor(process, Pid),
    Sub = {Pid, #sub{pid=Pid, ref=Ref}},
    Subs1 = [Sub|Subs0],
    {noreply, State#state{subs=Subs1}};

handle_cast({unsubscribe, Pid}, #state{chunk=Chunk, subs=Subs0}=State) ->
    lager:debug("Process ~p unsubscribed from chunk ~p.", [Pid, Chunk]),
    {Pid, #sub{ref=Ref}} = lists:keyfind(Pid, 1, Subs0),
    erlang:demonitor(Ref),
    Subs1 = lists:keydelete(Pid, 1, Subs0),
    {noreply, State#state{subs=Subs1}};

handle_cast({publish, Event}, #state{subs=Subs}=State) ->
    do_publish(Event, Subs),
    {noreply, State};

handle_cast({add_creature, Cid, Pos}, #state{creatures=Creatures, subs=Subs, log=Log}=State) ->
    do_publish(#evt_creature_add{cid=Cid, pos=Pos}, Subs),
    Event = #evt_creature_add{cid=Cid, pos=Pos},
    Log1 = [Event | Log],
    Creature = #creature{cid=Cid, pos=Pos},
    {noreply, State#state{creatures=[{Cid, Creature}|Creatures], log=Log1}};

handle_cast({move_creature, Cid, Pos}, #state{creatures=Creatures0, subs=Subs, log=Log}=State) ->
    do_publish(#evt_creature_move{cid=Cid, pos=Pos}, Subs),
    {_, Creature0} = lists:keyfind(Cid, 1, Creatures0),
    Creature1 = Creature0#creature{pos=Pos},
    Creatures1 = lists:keyreplace(Cid, 1, Creatures0, {Cid, Creature1}),
    Event = #evt_creature_move{cid=Cid, pos=Pos},
    Log1 = [Event | Log], 
    {noreply, State#state{creatures=Creatures1, log=Log1}};

handle_cast({remove_creature, Cid}, #state{creatures=Creatures0, subs=Subs, log=Log}=State) ->
    do_publish(#evt_creature_remove{cid=Cid}, Subs),
    Creatures1 = lists:keydelete(Cid, 1, Creatures0),
    Event = #evt_creature_remove{cid=Cid},
    Log1 = [Event | Log],
    {noreply, State#state{creatures=Creatures1, log=Log1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal -------------------------------------------------------------------

%% @doc Publish an event to all the chunk's subscribers.
%% @private
do_publish(Event, Subs) ->
    lists:foreach(
      fun({Pid, _}) ->
              Pid ! {event, Event}
      end,
      Subs).

%% @doc Gets the id of the creature at the given position, or false.
%% @private
find_creature_by_pos([{Cid, #creature{pos=Pos}}|_Creatures], Pos) ->
    {ok, Cid};
find_creature_by_pos([_Creature|Creatures], Pos) -> 
    find_creature_by_pos(Creatures, Pos);
find_creature_by_pos([], _Pos) -> 
    false.

%% @doc Compute the floor of a number (think "Math.floor").
%% @private
%% Source: http://schemecookbook.org/Erlang/NumberRounding
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%%% Tests ----------------------------------------------------------------------

-ifdef(TEST).

child_spec_test() ->
    ?assertEqual({artifice_chunk_0_0,
                  {?MODULE, start_link, [{0,0}]},
                  permanent, brutal_kill, worker, [?MODULE]},
                 child_spec({0,0})).

registered_name_test() ->
    ?assertEqual('artifice_chunk_0_0', registered_name({0,0})),
    ?assertEqual('artifice_chunk_0_-1', registered_name({0,-1})).

do_publish_test() ->
    do_publish(some_event, [{self(), #sub{pid=self()}}]),
    receive X -> ?assertEqual({event, some_event}, X) end.

find_creature_by_pos_test() ->
    ?assertMatch({ok, _Pid}, start_link({0,0})),
    Cid = <<"mycid1">>,
    ?assertEqual(false, creature_at({0,0}, {0,0})),
    add_creature({0,0}, Cid, {0,0}),
    ?assertEqual({ok, Cid}, creature_at({0,0}, {0,0})).

-endif.
