-module(artifice_brain_nn).
-behaviour(artifice_brain).

-compile(export_all).

%%% artifice_brain callbacks
-export([random/0]).
-export([crossover/2]).
-export([mutate/1]).
-export([react/2]).

-include("common.hrl").

%% Network node count parameters. Refers to the input,
%% hidden and output layers respectively.
-define(INPUT_COUNT,  8).
-define(OUTPUT_COUNT, 5).
-define(HIDDEN_COUNT, 2).
-define(WEIGHT_COUNT, ?HIDDEN_COUNT * (?INPUT_COUNT + ?OUTPUT_COUNT)).

-define(FLOAT_BITS, 32). % Default IEEE 754 single precision float

-record(nn, {
          hidden :: [[float()]],
          output :: [[float()]]
         }).

%%% artifice_brain callbacks ---------------------------------------------------

random() ->
    random:seed(erlang:now()),
    Hidden = random_layer(?HIDDEN_COUNT, ?INPUT_COUNT),
    Output = random_layer(?OUTPUT_COUNT, ?HIDDEN_COUNT),
    #nn{hidden=Hidden,
        output=Output}.

crossover(Brain1, Brain2) ->
    Brain1Code = pack_brain(Brain1),
    Brain2Code = pack_brain(Brain2),
    SplitPt = random:uniform(size(Brain1Code)-1),
    <<Left:SplitPt/bytes, _/binary>> = Brain1Code,
    <<_:SplitPt/bytes, Right/binary>> = Brain2Code,
    unpack_brain(<<Left/binary, Right/binary>>).

mutate(Brain) ->
    BrainCode = pack_brain(Brain),
    %% Pick a random byte and complement it.
    N = random:uniform(size(BrainCode)-1)-1,
    <<Left:N/bytes, V, Right/binary>> = BrainCode,
    unpack_brain(<<Left/binary, bnot V, Right/binary>>).

react(Brain, Percept) ->
    {pid, Pid} = lists:keyfind(pid, 1, Percept),
    {energy, Energy} = lists:keyfind(energy, 1, Percept),
    {pos, {X, Y}=Pos} = lists:keyfind(pos, 1, Percept),
    {creatures, Creatures} = lists:keyfind(creatures, 1, Percept),
    {food, Food} = lists:keyfind(food, 1, Percept),
    {CX, CY} = vector_to_nearest(Pos, [C#creature.pos || C <- Creatures]),
    {FX, FY} = vector_to_nearest(Pos, [F#food.pos || F <- Food]),
    [N, S, W, E, Eat] = activate_network([Energy,
                                          X, Y,
                                          CX, CY,
                                          FX, FY,
                                          length(Creatures)],
                                         Brain),
    %% Movement
    if
        N >= 1 -> artifice_creature:move(Pid, north);
        S >= 1 -> artifice_creature:move(Pid, south);
        W >= 1 -> artifice_creature:move(Pid, west);
        E >= 1 -> artifice_creature:move(Pid, east);
        true   -> ok
    end,
    %% Eating
    if
        Eat >= 1 -> artifice_creature:eat(Pid);
        true     -> ok
    end.

%%% Internal -------------------------------------------------------------------

activate_network(Inputs, #nn{hidden=Hidden, output=Output}) ->
    HOut = activate_layer(Inputs, Hidden),
    activate_layer(HOut, Output).

activate_layer(Inputs, Weights) ->
    [activate_neuron(Inputs, W) || W <- Weights].

activate_neuron(Inputs, Weights) ->
    actually_activate_neuron(Inputs, Weights, 0.0).

actually_activate_neuron([I|Is], [W|Ws], Sum) ->
    actually_activate_neuron(Is, Ws, W*I + Sum);
actually_activate_neuron([], [], Sum) ->
    Sum.

random_layer(NumNeurons, NumInputs) ->
    [random_neuron(NumInputs) || _ <- lists:seq(1, NumNeurons)].

random_neuron(NumWeights) ->
    [random_weight() || _ <- lists:seq(1, NumWeights)].

random_weight() ->
    (random:uniform()-0.5) * 10.0. % TODO twiddle for glory

pack_brain(#nn{hidden=Hidden, output=Output}) ->
    <<(pack_layer(Hidden))/binary, (pack_layer(Output))/binary>>.

pack_layer(Neurons) ->
    << <<(pack_floats(Weights))/binary>> || Weights <- Neurons >>.

pack_floats(Floats) ->
    actually_pack_floats(Floats, <<>>).

actually_pack_floats([F|Fs], Acc) ->
    actually_pack_floats(Fs, <<Acc/binary, F:?FLOAT_BITS/float>>);
actually_pack_floats([], Acc) ->
    Acc.

unpack_brain(Buffer) ->
    {Hidden, Rest} = unpack_layer(Buffer, ?HIDDEN_COUNT, ?INPUT_COUNT),
    {Output, <<>>} = unpack_layer(Rest, ?OUTPUT_COUNT, ?HIDDEN_COUNT),
    #nn{hidden=Hidden, output=Output}.

unpack_layer(Buffer, NumNodes, WeightsPerNode) ->
    actually_unpack_layer(Buffer, NumNodes, WeightsPerNode, []).

actually_unpack_layer(Buffer, 0, _WeightsPerNode, Acc) ->
    {lists:reverse(Acc), Buffer};
actually_unpack_layer(Buffer, NumNodes, WeightsPerNode, Acc) ->
    {Node, Rest} = unpack_floats(WeightsPerNode, Buffer),
    actually_unpack_layer(Rest, NumNodes-1, WeightsPerNode, [Node|Acc]).

unpack_floats(N, Buf) ->
    actually_unpack_floats(N, Buf, []).

actually_unpack_floats(0, Buf, Acc) ->
    {lists:reverse(Acc), Buf};
actually_unpack_floats(N, <<F:?FLOAT_BITS/float, Rest/binary>>, Acc) ->
    actually_unpack_floats(N-1, Rest, [F|Acc]).

vector_to_nearest({X1, Y1}=MyPos, [P|Ps]) ->
    {X2, Y2} =
        lists:foldl(
          fun(ItsPos, LeaderPos) ->
                  ItsDist    = artifice_util:squared_distance(MyPos, ItsPos),
                  LeaderDist = artifice_util:squared_distance(MyPos, LeaderPos),
                  case ItsDist < LeaderDist of
                      true  -> ItsPos;
                      false -> LeaderPos
                  end
          end,
          P,
          Ps),
    {X1-X2, Y1-Y2};
vector_to_nearest(_MyPos, []) ->
    {0, 0}. % TODO placeholder vector    
