-module(subs).

-include("inc.hrl").

-compile([export_all, nowarn_export_all]).

a(L) ->
    L ++ [1, 2, 3] ++ ?FRUITS.
