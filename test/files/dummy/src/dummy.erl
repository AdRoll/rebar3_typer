-module(dummy).

-include("inc.hrl").
-include("inc2.hrl").

-compile([export_all, nowarn_export_all]).

a(L) ->
    L ++ [1, 2, 3] ++ ?FRUITS ++ ?MORE.
