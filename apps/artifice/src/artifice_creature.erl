-module(artifice_creature).
-behaviour(gen_server).

%%% API
-export([start_link/2]).
-export([start_supervised/2]).
-export([child_spec/2]).
-export([new_cid/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          cid :: binary(),
          pos :: {integer(), integer()}
         }).

-define(THINK_HZ, 1).
-define(THINK_MESSAGE, think).

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
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [?MODULE],
    {Cid, StartFunc, Restart, Shutdown, Type, Modules}.

%% @doc Return a unique creature identifier.
new_cid() ->
    base64:encode(crypto:rand_bytes(6)).

%%% gen_server callbacks -------------------------------------------------------

init([Cid, Pos]) ->
    State = #state{cid=Cid, pos=Pos},
    add_to_initial_chunk(State),
    reset_timer(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(?THINK_MESSAGE, State0) ->
    State1 = think(State0),
    reset_timer(),
    {noreply, State1};
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

%% @doc Take a random action.
%% @private
think(#state{pos={X,Y}}=State) ->
    DeltaX = random:uniform(3)-2, %% -1..1
    DeltaY = random:uniform(3)-2, %% -1..1
    move({X+DeltaX, Y+DeltaY}, State).

%% @doc Add ourselves to the chunk we spawned in.
%% @private
add_to_initial_chunk(State) ->
    Chunk = artifice_chunk:chunk_at(State#state.pos),
    artifice_chunk:add_creature(Chunk, State#state.cid, State#state.pos),
    artifice_chunk:subscribe_initial(State#state.pos),
    ok.

%% @doc Move to a new position, updating the chunks' creature lists as needed.
%% @private
move(NewPos, State) ->
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
    artifice_chunk:update_subscriptions(OldPos, NewPos),
    State#state{pos=NewPos}.
