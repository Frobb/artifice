-module(artifice_creature).
-behaviour(gen_server).

%%% API
-export([start_link/3]).
-export([start_supervised/2, start_supervised/3]).
-export([child_spec/3]).
-export([get_brain/1]).
-export([new_cid/0]).
-export([move/2]).
-export([eat/1]).
-export([mate/1]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("common.hrl").
-include("event.hrl").

-record(state, {
          cid   :: binary(),
          pos   :: {integer(), integer()},
          brain :: artifice_brain:brain(),
          energy :: non_neg_integer(),
          known_creatures = [] :: [{binary(), #creature{}}],
          known_food = [] :: [#food{}]
         }).

-define(THINK_HZ, 1).
-define(THINK_MESSAGE, think).
-define(BRAIN, artifice_brain_nn).

%%% API ------------------------------------------------------------------------

%% @doc Start a new creature process.
start_link(Cid, Pos, Brain) ->
    gen_server:start_link(?MODULE, [Cid, Pos, Brain], []).

%% @doc Start a creature process in the creature's supervisor tree.
start_supervised(Cid, Pos) ->
    start_supervised(Cid, Pos, ?BRAIN:random()).

%% @doc Start a creature process in the creature supervisor's tree.
start_supervised(Cid, Pos, Brain) ->
    artifice_creature_sup:start_child(child_spec(Cid, Pos, Brain)).

%% @doc Generate a child spec for starting a chunk server from a supervisor.
child_spec(Cid, Pos, Brain) ->
    StartFunc = {?MODULE, start_link, [Cid, Pos, Brain]},
    Restart = transient,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [?MODULE],
    {Cid, StartFunc, Restart, Shutdown, Type, Modules}.

%% @doc Return a unique creature identifier.
new_cid() ->
    base64:encode(crypto:rand_bytes(6)).

%% @doc Move the creature in the specified direction.
move(Pid, Dir) ->
    gen_server:cast(Pid, {move, Dir}).

%% @doc Eat food at the current position.
eat(Pid) ->
    gen_server:cast(Pid, eat).

%% @doc Mate with the first creature at the current position (except self).
mate(Pid) ->
    gen_server:cast(Pid, mate).

%% @doc Get the brain of the given creature.
get_brain(Pid) ->
    gen_server:call(Pid, get_brain).

%%% gen_server callbacks -------------------------------------------------------

init([Cid, Pos, Brain]) ->
    State = #state{cid=Cid,
                   pos=Pos,
                   brain=Brain,
                   energy=artifice_config:initial_energy()},
    add_to_initial_chunk(State),
    reset_timer(),
    {ok, State}.

handle_call(get_brain, _From, #state{brain=Brain}=State) ->
    {reply, Brain, State}.

handle_cast({move, Dir}, #state{pos={X, Y}}=State0) ->
    State1 = case Dir of
                 north -> actually_move({X, Y-1}, State0);
                 south -> actually_move({X, Y+1}, State0);
                 east  -> actually_move({X+1, Y}, State0);
                 west  -> actually_move({X-1, Y}, State0)
             end,
    {noreply, State1};

handle_cast(eat, #state{cid=Cid, pos=Pos, energy=Energy0}=State) ->
    case artifice_chunk:remove_food(Pos) of
        ok ->
            lager:debug("Creature '~s' ate some food.", [Cid]),
            Energy1 = min(artifice_config:initial_energy(),
                          Energy0 + artifice_config:food_energy()),
            {noreply, State#state{energy=Energy1}};
        error ->
            {noreply, State}
    end;

handle_cast(mate, #state{cid=MyCid, pos=Pos, brain=MyBrain}=State) ->
    case find_first_other(MyCid, artifice_chunk:creatures_at(Pos)) of
        {ok, MateCid} ->
            MatePid = artifice_creature_registry:whereis(MateCid),
            MateBrain = get_brain(MatePid),
            OffspringBrain = ?BRAIN:crossover(MyBrain, MateBrain),
            start_supervised(new_cid(), Pos, OffspringBrain),
            lager:info("Creature '~s' mated with '~s'.", [MyCid, MateCid]),
            {noreply, State}; % TODO energy drain
        error ->
            {noreply, State}
    end.

handle_info(?THINK_MESSAGE, State0) ->
    State1 = drain_energy(State0, energy_cost(ambient)),
    %% Did we die?
    case State1#state.energy >= 0 of
        true ->
            ?BRAIN:react(State1#state.brain, make_percept(State1)),
            reset_timer(),
            {noreply, State1};
        false ->
            Chunk = artifice_chunk:chunk_at(State1#state.pos),
            artifice_chunk:publish(Chunk, #evt_creature_die{
                                     cid=State1#state.cid
                                    }),
            artifice_chunk:add_food(State1#state.pos, carcass),
            lager:debug("Creature ~s died.", [State1#state.cid]),
            {stop, normal, State1}
    end;
handle_info({event, Event}, State0) ->
    State1 = handle_event(Event, State0),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cid=Cid, pos=Pos}) ->
    Chunk = artifice_chunk:chunk_at(Pos),
    artifice_chunk:remove_creature(Chunk, Cid),
    artifice_chunk:unsubscribe_final(Pos),
    artifice_creature_registry:unregister(Cid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Event handling -------------------------------------------------------------

handle_event(#evt_creature_add{cid=Cid, pos=Pos},
             #state{known_creatures=Creatures}=State) ->
    State#state{known_creatures=[{Cid, #creature{cid=Cid, pos=Pos}}|Creatures]};
handle_event(#evt_creature_move{cid=Cid, pos=Pos},
            #state{known_creatures=Creatures0}=State) ->
    Creatures1 = lists:keyreplace(Cid, 1, Creatures0, #creature{cid=Cid, pos=Pos}),
    State#state{known_creatures=Creatures1};
handle_event(#evt_creature_remove{cid=Cid},
            #state{known_creatures=Creatures}=State) ->
    State#state{known_creatures=lists:keydelete(Cid, 1, Creatures)};
handle_event(#evt_food_add{pos=Pos, type=Type}, #state{known_food=Food}=State) ->
    State#state{known_food=[#food{pos=Pos, type=Type}|Food]};
handle_event(#evt_food_remove{pos=Pos}, #state{known_food=Food0}=State) ->
    Food1 = lists:keydelete(Pos, 2, Food0), %% XXX: Relying on record structure!
    State#state{known_food=Food1};
handle_event(_Event, State) ->
    State.

%%% Internal -------------------------------------------------------------------

reset_timer() ->
    Time = 1000 div (?THINK_HZ * artifice_config:simulation_rate()),
    {ok, _TRef} = timer:send_after(Time, ?THINK_MESSAGE),
    ok.

%% @doc Add ourselves to the chunk we spawned in.
%% @private
add_to_initial_chunk(State) ->
    Chunk = artifice_chunk:chunk_at(State#state.pos),
    artifice_chunk:add_creature(Chunk, State#state.cid, State#state.pos),
    artifice_chunk:subscribe_initial(State#state.pos),
    artifice_creature_registry:register(State#state.cid, self()),
    ok.

%% @doc Move to a new position, updating the chunks' creature lists as needed.
%% @private
actually_move(NewPos, State0) ->
    State1 = drain_energy(State0, energy_cost(move)),
    Cid = State1#state.cid,
    OldPos = State1#state.pos,
    NewChunk = artifice_chunk:chunk_at(NewPos),
    OldChunk = artifice_chunk:chunk_at(OldPos),
    case NewChunk == OldChunk of
        false ->
            artifice_chunk:remove_creature(OldChunk, Cid),
            artifice_chunk:add_creature(NewChunk, Cid, NewPos);            
        true ->
            artifice_chunk:move_creature(NewChunk, Cid, NewPos)
    end,
    artifice_chunk:update_subscriptions(OldPos, NewPos),
    State1#state{pos=NewPos}.

%% @doc Apply ambient energy loss to the creature.
%% @private
drain_energy(#state{energy=Energy0}=State, Amount) ->
    Energy1 = Energy0 - Amount,
    State#state{energy=Energy1}.

%% @doc Get the energy cost for an action.
%% @private
energy_cost(Action) ->
    Costs = artifice_config:energy_costs(),
    {_, Cost} = lists:keyfind(Action, 1, Costs),
    Cost.

%% @doc Build a percept for the brain from the information known right now.
%% @private
make_percept(#state{energy=Energy,
                    pos=Pos,
                    known_creatures=Creatures,
                    known_food=Food}) ->
    [{pid, self()},
     {energy, Energy},
     {pos, Pos},
     {creatures, [C || {_, C} <- Creatures]},
     {food, Food}].

%% @doc Find the first cid that doesn't match the given cid, or error.
%% @private
find_first_other(MyCid, [MyCid|Cids]) ->
    find_first_other(MyCid, Cids);
find_first_other(_MyCid, [Cid|_Cids]) ->
    {ok, Cid};
find_first_other(_MyCid, []) ->
    error.
