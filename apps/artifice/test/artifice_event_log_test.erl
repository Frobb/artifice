-module(artifice_event_log_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/event.hrl").

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
              Log0 = artifice_event_log:new(),
              Log1 = artifice_event_log:add(#evt_creature_add{}, Log0),
              ?assertEqual(1, artifice_event_log:length(Log1)),
              ?assertEqual([#evt_creature_add{}],
                           artifice_event_log:to_list(Log1))
      end}
    ].
