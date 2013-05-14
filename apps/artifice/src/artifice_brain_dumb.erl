-module(artifice_brain_dumb).
-behaviour(artifice_brain).

%%% artifice_brain callbacks
-export([random/0]).
-export([crossover/2]).
-export([mutate/1]).
-export([react/2]).

-record(brain, {
	  str = [] :: list(),
	  armor = [] :: list()
	 }).

-define(DNA_LENGTH, 1023).
-define(LISTLENGTH, 10).

%%% artifice_brain callbacks ---------------------------------------------------
random() ->
    StrRand = random:uniform(?DNA_LENGTH),
    ArmorRand = random:uniform(?DNA_LENGTH),
    StrList = erlang:integer_to_list(StrRand, 2),
    ListLength = list_length(StrList),
    if ListLength < ?LISTLENGTH ->
	    ModStrList = add_zeros(StrList, ?LISTLENGTH-ListLength);
       true ->
	    ModStrList = StrList
    end,
    ArmorList = erlang:integer_to_list(ArmorRand, 2), 
    ListLength1 = list_length(ArmorList),
    if ListLength1 < ?LISTLENGTH ->
	    ModArmorList = add_zeros(ArmorList, ?LISTLENGTH-ListLength1);
       true ->
	    ModArmorList = ArmorList
    end,
    #brain{str=ModStrList, armor=ModArmorList}.

crossover(B1, B2) ->
    {Str, Armor} = {B1#brain.str, B2#brain.armor},
    {Str2, Armor2} = {B2#brain.str, B2#brain.armor},
    B1Brain = lists:append(Str, Armor),
    B2Brain = lists:append(Str2, Armor2),
    %% OP Crossover
    HalfListLength = trunc(list_length(B1Brain)/2),
    ListLength = list_length(B1Brain),
    PointToRandom = trunc(random:uniform(ListLength)),
    {L1, L2} = swap(B1Brain, B2Brain, PointToRandom),
    ToChose = random:uniform(2),
    if ToChose > 1 ->
	    NewBrain = #brain{}, 
	    NewBrain#brain{str = lists:sublist(L1, 1, HalfListLength), 
			   armor = lists:sublist(L1, HalfListLength + 1, ListLength)};
       true ->
	    NewBrain = #brain{}, 
	    NewBrain#brain{str = lists:sublist(L2, 1, HalfListLength), 
			   armor = lists:sublist(L2, HalfListLength + 1, ListLength)}
    end.
    
mutate(B) ->
    B.

react(_B, Percept) ->
    Pid = my_pid(Percept),
    artifice_creature:move(Pid, random_element([north, south, east, west])).

%%% Internal -------------------------------------------------------------------
%% @doc Swaps the content between two lists form index Point.
%% @private
swap(List1, List2, Point) ->
    SubList1 = lists:sublist(List1, 1, Point),
    SubsubList1 = lists:sublist(List1, Point+1, list_length(List1)),
    SubList2 = lists:sublist(List2, 1, Point),
    SubsubList2 = lists:sublist(List2, Point+1, list_length(List2)),
    NewList1 = lists:append(SubList1, SubsubList2),
    NewList2 = lists:append(SubList2, SubsubList1),
    {NewList1, NewList2}.

%% @doc Counts the length of List.
%% @private
list_length(List) ->
    list_length(List, 0).

list_length([], Acc) -> Acc;

list_length(List, Acc) ->
    list_length(tl(List), Acc+1).

%% @doc Adds N zeros to the front of List.
%% @private
add_zeros(List, 0) -> List;

add_zeros(List, N) ->
    List1 = [48 | List],
    add_zeros(List1, N-1).

%% @doc Get the Pid of the creature we're managing.
%% @private
my_pid(Percept) ->
    {pid, Pid} = lists:keyfind(pid, 1, Percept),
    Pid.

%% @doc Choose a random element in a list.
%% @private
random_element(List) ->
    lists:nth(random:uniform(length(List)), List).
