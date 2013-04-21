-module(artifice_creature).
-behaviour(gen_server).

%%% API
-export([start_link/2]).
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

%%% API ------------------------------------------------------------------------

%% @doc Start a new creature process.
start_link(Cid, Pos) ->
    gen_server:start_link(?MODULE, [Cid, Pos], []).

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
    {ok, #state{cid=Cid, pos=Pos}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
