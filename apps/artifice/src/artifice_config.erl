-module(artifice_config).

%%% API
-export([simulation_rate/0, simulation_rate/1]).

-define(APP, artifice).

%%% API ------------------------------------------------------------------------

%% @doc Get the current simulation rate.
simulation_rate() ->
    {ok, Rate} = application:get_env(?APP, simulation_rate),
    Rate.

%% @doc Set the current simulation rate.
simulation_rate(Rate) ->
    application:set_env(?APP, simulation_rate, Rate).
