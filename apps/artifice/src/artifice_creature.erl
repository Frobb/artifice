-module(artifice_creature).
-behaviour(gen_server).

%%% API
-export([start_link/2]).
-export([start_supervised/2]).
-export([child_spec/2]).
-export([new_cid/0]).
-export([move/2]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          cid   :: binary(),
          pos   :: {integer(), integer()},
          brain :: artifice_brain:brain(),
          energy :: non_neg_integer()
         }).

-define(THINK_HZ, 1).
-define(THINK_MESSAGE, think).
-define(BRAIN, artifice_brain_dumb).

-include("event.hrl").

%%% API ------------------------------------------------------------------------

%% @doc Start a new creature process.
start_link(Cid, Pos) ->
    gen_server:start_link(?MODULE, [Cid, Pos], []).

%% @doc Start a creature process in the creature supervisor's tree.
start_supervised(Cid, Pos) ->
    artifice_creature_sup:start_child(child_spec(Cid, Pos)).

%% @doc Generate a child spec for starting a chunk server from a supervisor.
child_spec(Cid, Pos) ->
    StartFunc = {?MODULE, start_link, [Cid, Pos]},
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

%%% gen_server callbacks -------------------------------------------------------

init([Cid, Pos]) ->
    State = #state{cid=Cid,
                   pos=Pos,
                   brain=?BRAIN:random(),
                   energy=artifice_config:initial_energy()},
    add_to_initial_chunk(State),
    reset_timer(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({move, Dir}, #state{pos={X, Y}}=State0) ->
    State1 = case Dir of
                 north -> actually_move({X, Y-1}, State0);
                 south -> actually_move({X, Y+1}, State0);
                 east  -> actually_move({X+1, Y}, State0);
                 west  -> actually_move({X-1, Y}, State0)
             end,
    {noreply, State1}.

handle_info(?THINK_MESSAGE, State0) ->
    State1 = drain_energy(State0, energy_cost(ambient)),
    %% Did we die?
    case State1#state.energy >= 0 of
        true ->
            ?BRAIN:react(State1#state.brain, [{pid, self()}]),
            reset_timer(),
            {noreply, State1};
        false ->
            Chunk = artifice_chunk:chunk_at(State1#state.pos),
            artifice_chunk:publish(Chunk, #evt_creature_die{
                                     cid=State1#state.cid
                                    }),
            lager:debug("Creature ~s died.", [State1#state.cid]),
            {stop, normal, State1}
    end;
handle_info({event, Event}, State) ->
    handle_event(Event),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cid=Cid, pos=Pos}) ->
    Chunk = artifice_chunk:chunk_at(Pos),
    artifice_chunk:remove_creature(Chunk, Cid),
    artifice_chunk:unsubscribe_final(Pos),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Event handling -------------------------------------------------------------

handle_event(_Event) ->
    ok.

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
