-module(artifice_event_log).

-include("event.hrl").

%%% API
-export([new/0]).
-export([add/2]).
-export([length/1]).
-export([to_list/1]).

-opaque log() :: list().
-type event() :: #evt_creature_add{}
               | #evt_creature_remove{}
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
    [Event|Log].

%% @doc Get the length of the log in number of events.
%% This may or may not be equal to the number of events actualy added,
%% as some events supersede earlier events.
length(Log) ->
    erlang:length(Log).

%% @doc Return all (still relevant) events in the log, ordered chronologically.
to_list(Log) ->
    lists:reverse(Log).
