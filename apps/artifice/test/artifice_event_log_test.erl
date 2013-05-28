-module(artifice_event_log_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/event.hrl").

-define(CHUNK, {0, 0}).
-define(CID, <<"mycid1">>).

new_test_() ->
    [
     {"A log has an initial length of 0",
      fun() ->
              ?assertEqual(0,
                           artifice_event_log:length(
                             artifice_event_log:new()))
      end},
     {"A log's list for is initially the empty list",
      fun() ->
              ?assertEqual([],
                           artifice_event_log:to_list(
                             artifice_event_log:new()))
      end}
    ].

add_test_() ->
    [
     {"Adding an event increases length and affects list form",
      fun() ->
	      % Testing size for 1 event.
              Log0 = artifice_event_log:new(),
              Log1 = artifice_event_log:add(#evt_food_add{}, Log0),
	      ?assertEqual(1, artifice_event_log:length(Log1)),
	      % Test to remove an event which is not in the log and instead add this to the log.
	      Log2 = artifice_event_log:add(#evt_creature_move{cid=?CID}, Log1),
	      Log3 = artifice_event_log:add(#evt_food_add{pos=?CHUNK}, Log2), 
	      Log4 = artifice_event_log:add(#evt_creature_move{cid= <<"mycid2">>}, Log3),
	      ?assertEqual(4, artifice_event_log:length(Log4)),     
              ?assertEqual([#evt_food_add{}, #evt_creature_move{cid=?CID},
			    #evt_food_add{pos={0,0}}, #evt_creature_move{cid= <<"mycid2">>}],
                           artifice_event_log:to_list(Log4)),
	      % Test to remove and replace an event which is in the log. 
	      Log5 = artifice_event_log:add(#evt_creature_move{cid= <<"mycid2">>, pos={0,0}}, Log4),
	      ?assertEqual([#evt_food_add{}, #evt_creature_move{cid=?CID}, #evt_food_add{pos={0,0}}, 
			    #evt_creature_move{cid= <<"mycid2">>, pos={0,0}}], 
			   artifice_event_log:to_list(Log5)),

	      NewLog0 = artifice_event_log:new(),
              NewLog1 = artifice_event_log:add(#evt_food_add{}, NewLog0),
	      NewLog2 = artifice_event_log:add(#evt_food_remove{}, NewLog1),
	      ?assertEqual([], NewLog2),

	      NewLog3 = artifice_event_log:add(#evt_creature_add{cid=?CID, pos=?CHUNK}, NewLog2),
	      NewLog4 = artifice_event_log:add(#evt_creature_remove{cid=?CID}, NewLog3),
	      ?assertEqual([], NewLog4)
      end}
    ].
