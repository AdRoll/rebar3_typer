-module(specs).

-export([specced/0]).

-spec specced() -> x:y().
specced() ->
    true.
