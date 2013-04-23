-module(artifice_client).
-behaviour(cowboy_websocket_handler).
 
%%% Cowboy WebSocket callbacks
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("event.hrl").

-record(state, {
          x :: integer(), y :: integer()
         }).

%%% Cowboy WebSocket Callbacks -------------------------------------------------
 
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.
 
websocket_init(_TransportName, Req, _Opts) ->
    {X, Y} = {0, 0},
    subscribe_initial(X, Y),
    {ok, Req, #state{x=X, y=Y}}.
 
websocket_handle({text, Msg}, Req, State0) ->
    {Type, Payload} = decode_message(Msg),
    {ok, State1} = handle(Type, Payload, State0),
    {ok, Req, State1};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.
 
websocket_info({event, Event}, Req, State) ->
    JSON = encode_event(Event),
    {reply, {text, JSON}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.
 
websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%% Message handling -----------------------------------------------------------

handle(<<"move">>, Payload, State) ->
    {_, X} = lists:keyfind(<<"x">>, 1, Payload),
    {_, Y} = lists:keyfind(<<"y">>, 1, Payload),
    update_subscriptions({State#state.x, State#state.y}, {X, Y}),
    {ok, State#state{x=X, y=Y}};

handle(<<"creature_add">>, Payload, State) ->
    {_, X} = lists:keyfind(<<"x">>, 1, Payload),
    {_, Y} = lists:keyfind(<<"y">>, 1, Payload),
    artifice_creature:start_supervised(artifice_creature:new_cid(), {X,Y}),
    {ok, State}.

%%% Internal -------------------------------------------------------------------

%% @doc Manipulate chunk subscriptions based on movement.
%% @private
update_subscriptions({OldX, OldY}, {NewX, NewY}) ->
    OldChunk = artifice_chunk:chunk_at({OldX, OldY}),
    NewChunk = artifice_chunk:chunk_at({NewX, NewY}),
    OldAdj = gb_sets:from_list(artifice_chunk:adjacent_chunks(OldChunk)),
    NewAdj = gb_sets:from_list(artifice_chunk:adjacent_chunks(NewChunk)),
    ToSub = gb_sets:to_list(gb_sets:difference(NewAdj, OldAdj)),
    ToUnsub = gb_sets:to_list(gb_sets:difference(OldAdj, NewAdj)),
    lists:foreach(fun artifice_chunk:subscribe/1, ToSub),
    lists:foreach(fun artifice_chunk:unsubscribe/1, ToUnsub).    

%% @doc Sets up initial chunk descriptions.
%% @private
subscribe_initial(X, Y) ->
    Chunk = artifice_chunk:chunk_at({X, Y}),
    lists:foreach(fun artifice_chunk:subscribe/1,
                  artifice_chunk:adjacent_chunks(Chunk)).

%% @doc Encode an event to a JSON binary.
%% @private
encode_event(#evt_creature_add{cid=Cid, pos={X,Y}}) ->
    encode_message(<<"creature_add">>,
                   [{<<"cid">>, Cid},
                    {<<"pos">>, [{<<"x">>, X}, {<<"y">>, Y}]}]);
encode_event(#evt_creature_move{cid=Cid, pos={X,Y}}) ->
    encode_message(<<"creature_move">>,
                   [{<<"cid">>, Cid},
                    {<<"pos">>, [{<<"x">>, X}, {<<"y">>, Y}]}]);
encode_event(#evt_creature_remove{cid=Cid}) ->
    encode_message(<<"creature_remove">>, [{<<"cid">>, Cid}]).

%% @doc Encode a message to a JSON binary.
%% @private
encode_message(Type, Payload) ->
    jsx:encode([{<<"type">>, Type}, {<<"payload">>, Payload}]).

%% @doc Decode a JSON message.
%% @private
decode_message(Msg) ->
    JSON = jsx:decode(Msg),
    {_, Type} = lists:keyfind(<<"type">>, 1, JSON),
    {_, Payload} = lists:keyfind(<<"payload">>, 1, JSON),
    {Type, Payload}.
