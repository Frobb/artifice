-module(artifice_event_log).

-include("event.hrl").

%%% API
-export([new/0]).
-export([add/2]).
-export([length/1]).
-export([to_list/1]).
-export([send_to/2]).
-export([remove_evt/3]).
-export([remove_evt/2]).

-opaque log() :: list().
-type event() :: #evt_creature_add{}
               | #evt_creature_remove{}
	       | #evt_creature_move{}
               | #evt_creature_die{}
               | #evt_food_add{}
               | #evt_food_remove{}.

-export_type([log/0]).

%%% API ------------------------------------------------------------------------

%% @doc Create a new event log.
-spec new() -> log().
new() -> [].

%% @doc Add an event to the log, possibly replacing earlier events.
-spec add(event(), log()) -> log().
add(Event, Log) ->
    case Event of
        #evt_food_add{} ->
            [Event|Log];
        #evt_food_remove{pos=Pos} ->
            purge(#evt_food_add{pos=Pos}, Log);
	#evt_creature_move{} ->
	    Log1 = remove_evt(Event, Log),
	    [Event|Log1];
	#evt_creature_remove{} ->
	    remove_evt(Event, Log);
	#evt_creature_add{} ->
	    [Event|Log];
        _Discard ->
            Log % Throw away all non-food-events to avoid filling the log
    end.

%% @doc Get the length of the log in number of events.
%% This may or may not be equal to the number of events actualy added,
%% as some events supersede earlier events.
length(Log) ->
    erlang:length(Log).

%% @doc Return all (still relevant) events in the log, ordered chronologically.
to_list(Log) ->
    lists:reverse(Log).

%% @doc Sends each event as {event, Event} to Pid ordered chronologically.
send_to(Pid, Log) ->
    lists:foreach(
      fun(Event) ->
              Pid ! {event, Event}
      end,
      to_list(Log)).

%%% Internal -------------------------------------------------------------------

remove_evt(Event, List) ->
    remove_evt(Event, List, []).

remove_evt(Event, [], Acc) -> to_list(Acc);

remove_evt(Event, [H|TL]=Log, Acc) ->
    case Event of 
	#evt_creature_move{} ->
	    case H of 
		#evt_creature_move{} ->
		    NewCid = Event#evt_creature_move.cid,
		    OldCid = H#evt_creature_move.cid,
		    if NewCid == OldCid ->
			    remove_evt(Event, TL, Acc);
		       true  ->
			    remove_evt(Event, TL, [H|Acc])
		    end;
		_ -> remove_evt(Event, TL, [H|Acc])
	    end;
	#evt_creature_remove{} -> 
	    case H of 
		#evt_creature_add{} ->
		    RmCid = Event#evt_creature_remove.cid,
		    AddCid = H#evt_creature_add.cid,
		    if RmCid == AddCid ->
			    remove_evt(Event, TL, Acc);
		       true  ->
			    remove_evt(Event, TL, [H|Acc])
		    end;
		_ -> remove_evt(Event, TL, [H|Acc])
	    end;
	#evt_food_remove{} ->
	    case H of 
		#evt_food_add{} ->
		    RmPos = Event#evt_food_remove.pos,
		    AddPos = H#evt_food_add.pos,
		    if RmPos == AddPos ->
			    remove_evt(Event, TL, Acc);
		       true ->
			    remove_evt(Event, TL, [H|Acc])
		    end;
		_ -> remove_evt(Event, TL, [H|Acc])
	    end;
	_ -> Log
    end.

purge(Template, Log) ->
    lists:filter(fun(Event) -> Event /= Template end, Log).
