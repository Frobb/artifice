-module(artifice_chunk_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/event.hrl").

-define(CHUNK, {0, 0}).
-define(CID, <<"mycid1">>).

main_test_() ->
    {setup,
     fun() ->
             meck_artifice_config(),
             artifice_chunk_sup:start_link(),
             unlink(whereis(artifice_chunk_sup)),
             ok
     end,
     fun(_) ->
             unmeck_artifice_config(),
             erlang:exit(whereis(artifice_chunk_sup), kill)
     end,
     fun(_) ->
             [
              {"Subscribed processes get events",
               fun() ->
                       artifice_chunk:subscribe(?CHUNK),
                       artifice_chunk:publish(?CHUNK, #evt_food_add{}),
                       receive
                           X ->
                               ?assertEqual({event, #evt_food_add{}}, X)
                       end
               end},
              {"Once unsubscribed, events are no longer received",
               fun() ->
                       artifice_chunk:unsubscribe(?CHUNK),
                       artifice_chunk:publish(?CHUNK, #evt_food_add{}),
                       receive
                           {event, _} ->
                               ?assert(false)
                       after 100 ->
                               ok
                       end
               end},
              {"Initially a cell has no creatures",
               fun() ->
                       ?assertEqual([], artifice_chunk:creatures_at({0, 0}))
               end},
              {"Added creature is found at position",
               fun() ->
                       ?assertEqual(ok, artifice_chunk:add_creature(
                                          ?CHUNK, ?CID, {0, 0})
                                   ),
                       ?assertEqual([?CID], artifice_chunk:creatures_at({0, 0}))
               end},
	      {"Getting the event log",
	       fun() ->
		       ?assertEqual(#evt_food_add{},
                                    hd(artifice_chunk:event_log(?CHUNK))
				   )
	       end},
              {"Moving a creature updates cells",
               fun() ->
                       ?assertEqual(ok, artifice_chunk:move_creature(
                                          ?CHUNK, ?CID, {0, 1})
                                   ),
                       ?assertEqual([], artifice_chunk:creatures_at({0, 0})),
                       ?assertEqual([?CID], artifice_chunk:creatures_at({0, 1}))
               end},
              {"Removing a creature updates cells",
               fun() ->
                       ?assertEqual(ok, artifice_chunk:remove_creature(
                                          ?CHUNK, ?CID)
                                   ),
                       ?assertEqual([], artifice_chunk:creatures_at({0, 1}))
               end}
             ]
     end}.

adjacent_chunks_test() ->
    ?assertEqual(
       [{0, 0},  {0, -1}, {-1, -1},
        {-1, 0}, {-1, 1}, {0, 1},
        {1, 1},  {1,  0}, {1, -1}],
       artifice_chunk:adjacent_chunks(?CHUNK)).

chunk_at_test() ->
    ?assertEqual({0, 0}, artifice_chunk:chunk_at({0, 0})),
    ?assertEqual({-1, -1}, artifice_chunk:chunk_at({-1, -1})),
    ?assertEqual({1, 1}, artifice_chunk:chunk_at({128, 128})).

%%% ----------------------------------------------------------------------------

meck_artifice_config() ->
    meck:new(artifice_config),
    meck:expect(artifice_config, random_food, fun() -> 9999999999 end),
    meck:expect(artifice_config, initial_food, fun() -> 0 end).

unmeck_artifice_config() ->
    meck:unload(artifice_config).
