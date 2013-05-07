-module(artifice_brain).

-type brain()   :: any().
-type percept() :: [{atom(), any()}].

-export_type([brain/0]).
-export_type([percept/0]).

-callback random() -> brain().

-callback crossover(B1, B2) -> B3 when
      B1 :: brain(),
      B2 :: brain(),
      B3 :: brain().

-callback mutate(B1) -> B2 when
      B1 :: brain(),
      B2 :: brain().

-callback react(B, Percept) -> ok when
      B       :: brain(),
      Percept :: percept().
