-module(artifice_config).

%%% API
-export([simulation_rate/0, simulation_rate/1]).
-export([initial_energy/0, initial_energy/1]).
-export([energy_cost/1, energy_cost/2]).
-export([food_energy/0, food_energy/1]).
-export([initial_food/0, initial_food/1]).
-export([random_food/0, random_food/1]).

-define(APP, artifice).

%%% API ------------------------------------------------------------------------

%% @doc Get the current simulation rate.
simulation_rate() ->
    get_env(simulation_rate).

%% @doc Set the current simulation rate.
simulation_rate(Rate) ->
    set_env(simulation_rate, Rate).

%% @doc Get the initial energy available to a newly spawned creatures.
initial_energy() ->
    get_env(initial_energy).

%% @doc Set the initial energy available to newly spawned creatures.
initial_energy(InitialEnergy) ->
    set_env(initial_energy, InitialEnergy).

%% @doc Get the per-tick energy loss rate for creatures.
energy_cost(Action) ->
    {Action, Value} = lists:keyfind(Action, 1, get_env(energy_costs)),
    Value.

%% @doc Set the energy cost for a particular action.
energy_cost(Action, Value) ->
    Costs = lists:keyreplace(Action, 1, get_env(energy_costs), {Action, Value}),
    set_env(energy_costs, Costs).

%% @doc Get the amount of energy gained from eating food.
food_energy() ->
    get_env(food_energy).

%% @doc Set the amount of energy gained from eating food.
food_energy(Energy) ->
    set_env(food_energy, Energy).

%% @doc Get the initial food available in a newly spawned chunk.
initial_food() ->
    get_env(initial_food).

%% @doc Set the initial food available in a spawned chunk.
initial_food(InitialFood) ->
    set_env(initial_food, InitialFood).

%% @doc Get the rate of randomly spawned food.
random_food() ->
    get_env(spawn_food_rate).

%% @doc Set the rate of randomly spawned food. 
random_food(SetTimer) ->
    set_env(spawn_food_rate, SetTimer).

%%% Internal -------------------------------------------------------------------

%% @doc Get the value of an application environment variable.
%% @private
get_env(Key) ->
    {ok, Value} = application:get_env(?APP, Key),
    Value.

%% @doc Set the value of an application environment variable.
%% @private
set_env(Key, Value) ->
    application:set_env(?APP, Key, Value).
