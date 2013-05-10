-record(creature, {
          cid :: binary(),
          pos :: {integer(), integer()}
         }).

-record(food, {
          pos  :: {integer(), integer()},
          type :: noodles
         }).
