-module(artifice_util).

%%% API
-export([distance/2]).
-export([squared_distance/2]).

%%% API ------------------------------------------------------------------------

%% @doc Compute the Euclidian distance between two points.
distance(Pos1, Pos2) ->
    math:sqrt(squared_distance(Pos1, Pos2)).

%% @doc Compute the squared Euclidian distance between two points.
squared_distance({X1, Y1}, {X2, Y2}) ->
    DX = X1-X2,
    DY = Y1-Y2,
    DX*DX + DY*DY.
