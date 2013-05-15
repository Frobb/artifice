-define(CHUNK_WIDTH, 128).
-define(CHUNK_HEIGHT, 128).

-record(creature, {
          cid :: binary(),
          pos :: {integer(), integer()}
         }).

-record(food, {
          pos  :: {integer(), integer()},
          type :: noodles
         }).
