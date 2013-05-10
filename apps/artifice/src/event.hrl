-record(evt_creature_add, {
          cid :: binary(),
          pos :: {integer(), integer()} %% TODO: Make real type
         }).

-record(evt_creature_remove, {
          cid :: binary()
         }).

-record(evt_creature_move, {
          cid :: binary(),
          pos :: {integer(), integer()}
         }).

-record(evt_creature_die, {
          cid :: binary()
         }).

-record(evt_food_add, {
          pos  :: {integer(), integer()},
          type :: atom()
         }).

-record(evt_food_remove, {
          pos :: {integer(), integer()}
         }).
