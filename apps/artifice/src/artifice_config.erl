-module(artifice_config).

%%% API
-export([simulation_rate/0, simulation_rate/1]).
-export([initial_energy/0, initial_energy/1]).
-export([energy_costs/0, energy_costs/1]).
-export([food_energy/0, food_energy/1]).

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
energy_costs() ->
    get_env(energy_costs).

%% @doc Set the per-tick energy loss rate for creatures.
energy_costs(Costs) ->
    set_env(energy_costs, Costs).

%% @doc Get the amount of energy gained from eating food.
food_energy() ->
    get_env(food_energy).

%% @doc Set the amount of energy gained from eating food.
food_energy(Energy) ->
    set_env(food_energy, Energy).

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
