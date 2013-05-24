-module(artifice_creature).
-behaviour(gen_server).

%%% API
-export([start_link/3]).
-export([start_supervised/2, start_supervised/3]).
-export([child_spec/3]).
-export([new_cid/0]).
-export([move/2]).
-export([eat/1]).
-export([mate/1]).
-export([fight/1]).
-export([drain_energy/2]).
-export([kill/1]).

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
          known_food = [] :: [#food{}],
          last_mating :: erlang:timestamp()
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
    Restart = temporary,
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

%% @doc Request to mate with the first creature at
%% the current position (except self).
mate(Pid) ->
    gen_server:cast(Pid, mate).

%% @doc Fight with the first creature at the current position (except self).
fight(Pid) ->
    gen_server:cast(Pid, fight).

%% @doc Drain the specified amount of energy from the given creature.
drain_energy(Pid, Amount) ->
    gen_server:cast(Pid, {drain_energy, Amount}).

%% @doc Kill the given creature.
kill(Pid) ->
    gen_server:cast(Pid, kill).

%%% gen_server callbacks -------------------------------------------------------

init([Cid, Pos, Brain]) ->
    State = #state{cid=Cid,
                   pos=Pos,
                   brain=Brain,
                   energy=artifice_config:initial_energy(),
                   last_mating=erlang:now()},
    add_to_initial_chunk(State),
    reset_timer(),
    {ok, State}.

handle_call(get_brain, _From, #state{brain=Brain}=State) ->
    {reply, Brain, State}.

handle_cast({move, Dir}, #state{pos={X, Y}}=State0) ->
    State1 = case Dir of
                 north -> attempt_move({X, Y-1}, State0);
                 south -> attempt_move({X, Y+1}, State0);
                 east  -> attempt_move({X+1, Y}, State0);
                 west  -> attempt_move({X-1, Y}, State0)
             end,
    {noreply, State1};

handle_cast(eat, #state{cid=Cid, pos=Pos, energy=Energy0}=State) ->
    case artifice_chunk:remove_food(Pos) of
        ok ->
            lager:debug("Creature '~s' ate some food.", [Cid]),
            Energy1 = min(artifice_config:max_energy(),
                          Energy0 + artifice_config:food_energy()),
            {noreply, State#state{energy=Energy1}};
        error ->
            {noreply, State}
    end;

handle_cast(mate, #state{cid=MyCid, pos=Pos}=State) ->
    case find_first_other(MyCid, artifice_chunk:creatures_at(Pos)) of
        {ok, MateCid} ->
            case can_mate(State) of
                true ->
                    request_mate(
                      artifice_creature_registry:whereis(MateCid),
                      MyCid);
                false ->
                    ok
            end;
        error -> 
            ok
    end,
    {noreply, State};

handle_cast(fight, #state{cid=MyCid, pos=Pos}=State) ->
    case find_first_other(MyCid, artifice_chunk:creatures_at(Pos)) of
        {ok, EnemyCid} ->
            EnemyPid = artifice_creature_registry:whereis(EnemyCid),
            case random:uniform(2) of
                1 -> drain_energy(self(), artifice_config:energy_cost(fight));
                2 -> drain_energy(EnemyPid, artifice_config:energy_cost(fight))
            end,
            lager:info("Creature '~s' fought with '~s'.", [MyCid, EnemyCid]),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast({drain_energy, Amount}, #state{energy=Energy}=State) ->
    {noreply, State#state{energy=Energy - Amount}};

handle_cast(kill, State) ->
    {noreply, State#state{energy=0}};

handle_cast({request_mate, MateCid}, #state{cid=MyCid}=State) ->
    case can_mate(State) of
        true ->
            acknowledge_mate(artifice_creature_registry:whereis(MateCid), MyCid),
            drain_energy(self(), artifice_config:energy_cost(mate)),
            {noreply, State#state{last_mating=now()}};
        false ->
            {noreply, State}
    end;

handle_cast({acknowledge_mate, MateCid},
            #state{brain=MyBrain,
                   pos=Pos,
                   cid=MyCid}=State) ->
    drain_energy(self(), artifice_config:energy_cost(mate)),
    MateBrain = artifice_creature_registry:brain_of(MateCid),
    OffspringBrain = make_offspring_brain(MyBrain, MateBrain),
    start_supervised(new_cid(), Pos, OffspringBrain),
    lager:info("Creature '~s' mated with '~s'.", [MyCid, MateCid]),
    {noreply, State#state{last_mating=now()}}.

handle_info(?THINK_MESSAGE, State) ->
    drain_energy(self(), artifice_config:energy_cost(ambient)),
    %% Did we die?
    case State#state.energy >= 0 of
        true ->
            ?BRAIN:react(State#state.brain, make_percept(State)),
            reset_timer(),
            {noreply, State};
        false ->
            Chunk = artifice_chunk:chunk_at(State#state.pos),
            artifice_chunk:publish(Chunk, #evt_creature_die{
                                     cid=State#state.cid
                                    }),
            artifice_chunk:add_food(State#state.pos, carcass),
            lager:debug("Creature ~s died.", [State#state.cid]),
            {stop, normal, State}
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

%% @doc Request that the given creature mate with us.
request_mate(Pid, MyCid) ->
    gen_server:cast(Pid, {request_mate, MyCid}).

%% @doc Acknowledge a previous mating request.
acknowledge_mate(Pid, MyCid) ->
    gen_server:cast(Pid, {acknowledge_mate, MyCid}).

%% @doc Set the think timer to trigger one think period from now.
reset_timer() ->
    Time = 1000 div (?THINK_HZ * artifice_config:simulation_rate()),
    {ok, _TRef} = timer:send_after(Time, ?THINK_MESSAGE),
    ok.

%% @doc Add ourselves to the chunk we spawned in.
%% @private
add_to_initial_chunk(State) ->
    Chunk = artifice_chunk:chunk_at(State#state.pos),
    artifice_chunk:add_creature(Chunk, State#state.cid, State#state.pos),
    artifice_chunk:subscribe_initial(State#state.pos, false),
    artifice_creature_registry:register(State#state.cid,
                                        self(),
                                        State#state.pos,
                                        State#state.brain),
    ok.

%% @doc Attempt to move to a new position, updating the
%% chunks' creature lists as needed.
%% @private
attempt_move(NewPos, State) ->
    case validate_move(NewPos, State) of
        true  -> perform_move(NewPos, State);
        false -> State
    end.

%% @doc Return true if the requested move is valid and can be performed.
%% @private
validate_move(_NewPos, _State) ->
    true. % All moves are OK for now.

%% @doc Actually perform a move. Assumes it's valid.
%% @private
perform_move(NewPos, State) ->
    drain_energy(self(), artifice_config:energy_cost(move)),
    case artifice_config:wrap_spawn_chunk() of
        true  -> perform_wrapping_move(NewPos, State);
        false -> perform_normal_move(NewPos, State)
    end.

%% @doc Perform a normal, unrestricted move.
perform_normal_move(NewPos, State) ->
    Cid = State#state.cid,
    OldPos = State#state.pos,
    NewChunk = artifice_chunk:chunk_at(NewPos),
    OldChunk = artifice_chunk:chunk_at(OldPos),
    case NewChunk == OldChunk of
        false ->
            artifice_chunk:remove_creature(OldChunk, Cid),
            artifice_chunk:add_creature(NewChunk, Cid, NewPos);            
        true ->
            artifice_chunk:move_creature(NewChunk, Cid, NewPos)
    end,
    artifice_chunk:update_subscriptions(OldPos, NewPos, false),
    artifice_creature_registry:update_pos(Cid, NewPos),
    State#state{pos=NewPos}.

%% @doc Perform a move wrapped around the borders of the current chunk.
perform_wrapping_move({NewX, NewY}, #state{pos=OldPos}=State) ->
    {CX, CY} = artifice_chunk:chunk_at(OldPos),
    {MinX, MinY} = {CX*?CHUNK_WIDTH, CY*?CHUNK_HEIGHT},
    {MaxX, MaxY} = {(CX+1)*?CHUNK_WIDTH-1, (CY+1)*?CHUNK_HEIGHT-1},
    WrappedX = if NewX < MinX -> MaxX;
                  NewX > MaxX -> MinX;
                  true -> NewX
               end,
    WrappedY = if NewY < MinY -> MaxY;
                  NewY > MaxY -> MinY;
                  true -> NewY
               end,
    perform_normal_move({WrappedX, WrappedY}, State).

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

%% @doc Return true if mating is possible right now.
can_mate(#state{last_mating=LastMate}) ->
    seconds_between(LastMate, now()) >= artifice_config:mating_cooldown().

%% @doc Return the time in seconds from T1 to T2.
seconds_between(T1, T2) ->
    timer:now_diff(T2, T1) div 1000000 * artifice_config:simulation_rate(). % us

%% @doc Combine two brains to form an offspring brain.
%% @private
make_offspring_brain(Brain1, Brain2) ->
    Offspring = ?BRAIN:crossover(Brain1, Brain2),
    maybe_mutate(Offspring).

%% @doc Perform mutation or leave the brain unmolested based on fair dice roll.
%% @private
maybe_mutate(Brain) ->
    case random:uniform() < artifice_config:mutation_rate() of
        true  -> ?BRAIN:mutate(Brain);
        false -> Brain
    end.

