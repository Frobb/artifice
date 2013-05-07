-module(artifice_brain_dumb).
-behaviour(artifice_brain).

%%% artifice_brain callbacks
-export([random/0]).
-export([crossover/2]).
-export([mutate/1]).
-export([react/2]).

%%% artifice_brain callbacks ---------------------------------------------------

random() ->
    'i am smrt'.

crossover(B1, _B2) ->
    B1. %% Note that B1=:=B2

mutate(B) ->
    B.

react(_B, Percept) ->
    Pid = my_pid(Percept),
    artifice_creature:move(Pid, random_element([north, south, east, west])).

%%% Internal -------------------------------------------------------------------

%% @doc Get the Pid of the creature we're managing.
%% @private
my_pid(Percept) ->
    {pid, Pid} = lists:keyfind(pid, 1, Percept),
    Pid.

%% @doc Choose a random element in a list.
%% @private
random_element(List) ->
    lists:nth(random:uniform(length(List)), List).
