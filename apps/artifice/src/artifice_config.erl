-module(artifice_config).

%%% API
-export([simulation_rate/0, simulation_rate/1]).
-export([initial_energy/0, initial_energy/1]).
-export([energy_loss_rate/0, energy_loss_rate/1]).

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
energy_loss_rate() ->
    get_env(energy_loss_rate).

%% @doc Set the per-tick energy loss rate for creatures.
energy_loss_rate(Rate) ->
    set_env(energy_loss_rate, Rate).

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
