-module(artifice).

-export([start/0]).

start() ->
    lager:start(),
    reloader:start_link(),
    start_graph(artifice).

%% @doc Start an application and all of its dependencies.
%% @private
start_graph(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, AppDep}} ->
            start_graph(AppDep),
            start_graph(App) % Try again
    end.
