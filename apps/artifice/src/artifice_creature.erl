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
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cid=Cid, pos=Pos}) ->
    GridRef = artifice_chunk:gridref_of(Pos),
    artifice_chunk:remove_creature(GridRef, Cid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    GridRef = artifice_chunk:gridref_of(State#state.pos),
    artifice_chunk:add_creature(GridRef, State#state.cid, State#state.pos).

%% @doc Move to a new position, updating the chunks' creature lists as needed.
%% @private
move(NewPos, State) ->
    Cid = State#state.cid,
    OldPos = State#state.pos,
    NewGrid = artifice_chunk:gridref_of(NewPos),
    OldGrid = artifice_chunk:gridref_of(OldPos),
    case NewGrid == OldGrid of
        true ->
            artifice_chunk:remove_creature(OldGrid, Cid),
            artifice_chunk:add_creature(NewGrid, Cid, NewPos);            
        false ->
            artifice_chunk:move_creature(NewGrid, Cid, NewPos)
    end,
    State#state{pos=NewPos}.
