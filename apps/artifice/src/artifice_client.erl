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
          pos :: {integer(), integer()}
         }).

%%% Cowboy WebSocket Callbacks -------------------------------------------------
 
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.
 
websocket_init(_TransportName, Req, _Opts) ->
    Pos = {0, 0},
    artifice_chunk:subscribe_initial(Pos, true),
    {ok, Req, #state{pos=Pos}}.
 
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
 
websocket_terminate(_Reason, _Req, State) ->
    artifice_chunk:unsubscribe_final(State#state.pos),
    ok.

%%% Message handling -----------------------------------------------------------

handle(<<"move">>, Payload, State) ->
    {_, X} = lists:keyfind(<<"x">>, 1, Payload),
    {_, Y} = lists:keyfind(<<"y">>, 1, Payload),
    artifice_chunk:update_subscriptions(State#state.pos, {X, Y}, true),
    {ok, State#state{pos={X, Y}}};

handle(<<"creature_add">>, Payload, State) ->
    {_, X} = lists:keyfind(<<"x">>, 1, Payload),
    {_, Y} = lists:keyfind(<<"y">>, 1, Payload),
    artifice_creature:start_supervised(artifice_creature:new_cid(), {X, Y}),
    {ok, State};

handle(<<"food_add">>, Payload, State) ->
    {_, X} = lists:keyfind(<<"x">>, 1, Payload),
    {_, Y} = lists:keyfind(<<"y">>, 1, Payload),
    artifice_chunk:add_food({X, Y}, noodles),
    {ok, State}.

%%% Internal -------------------------------------------------------------------

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
    encode_message(<<"creature_remove">>, [{<<"cid">>, Cid}]);
encode_event(#evt_creature_die{cid=Cid}) ->
    encode_message(<<"creature_die">>, [{<<"cid">>, Cid}]);
encode_event(#evt_food_add{pos={X, Y}, type=Type}) ->
    TypeName = list_to_binary(atom_to_list(Type)),
    encode_message(<<"food_add">>,
                   [{<<"pos">>,
                     [{<<"x">>, X},
                     {<<"y">>, Y}]},
                    {<<"type">>, TypeName}]);
encode_event(#evt_food_remove{pos={X, Y}}) ->
    encode_message(<<"food_remove">>,
                   [{<<"pos">>,
                     [{<<"x">>, X}, {<<"y">>, Y}]}]).

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
