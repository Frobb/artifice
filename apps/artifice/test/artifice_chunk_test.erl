-module(artifice_chunk_test).

-include_lib("eunit/include/eunit.hrl").

-define(CHUNK, {0, 0}).
-define(CID, <<"mycid1">>).

main_test_() ->
    {setup,
     fun() ->
             artifice_chunk_sup:start_link(),
             unlink(whereis(artifice_chunk_sup)),
             ok
     end,
     fun(_) ->
             erlang:exit(whereis(artifice_chunk_sup), kill)
     end,
     fun(_) ->
             [
              {"Subscribed processes get events",
               fun() ->
                       artifice_chunk:subscribe(?CHUNK),
                       artifice_chunk:publish(?CHUNK, my_event),
                       receive
                           X ->
                               ?assertEqual({event, my_event}, X)
                       end
               end},
              {"Once unsubscribed, events are no longer received",
               fun() ->
                       artifice_chunk:unsubscribe(?CHUNK),
                       artifice_chunk:publish(?CHUNK, my_event),
                       receive
                           {event, _} ->
                               ?assert(false)
                       after 100 ->
                               ok
                       end
               end},
              {"Initially a cell has no creatures",
               fun() ->
                       ?assertEqual(false, artifice_chunk:creature_at(
                                             ?CHUNK, {0, 0})
                                   )
               end},
              {"Added creature is found at position",
               fun() ->
                       ?assertEqual(ok, artifice_chunk:add_creature(
                                          ?CHUNK, ?CID, {0, 0})
                                   ),
                       ?assertEqual({ok, ?CID}, artifice_chunk:creature_at(
                                                  ?CHUNK, {0, 0})
                                   )
               end},
	      {"Getting the event log",
	       fun() ->
		       ?assertEqual(my_event,
                                    hd(artifice_chunk:event_log(?CHUNK))
				   )
	       end},
              {"Moving a creature updates cells",
               fun() ->
                       ?assertEqual(ok, artifice_chunk:move_creature(
                                          ?CHUNK, ?CID, {0, 1})
                                   ),
                       ?assertEqual(false, artifice_chunk:creature_at(
                                             ?CHUNK, {0, 0})
                                   ),
                       ?assertEqual({ok, ?CID}, artifice_chunk:creature_at(
                                                  ?CHUNK, {0, 1})
                                   )
               end},
              {"Removing a creature updates cells",
               fun() ->
                       ?assertEqual(ok, artifice_chunk:remove_creature(
                                          ?CHUNK, ?CID)
                                   ),
                       ?assertEqual(false, artifice_chunk:creature_at(
                                             ?CHUNK, {0, 1})
                                   )
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
