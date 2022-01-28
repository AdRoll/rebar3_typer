-module(dummy).

-include("inc.hrl").
-include("inc2.hrl").

-export([a/0]).

a() ->
    ?FRUITS ++ ?MORE.
