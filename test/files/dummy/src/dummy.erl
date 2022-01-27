-module(dummy).

-compile([export_all, nowarn_export_all]).

a(L) ->
    L ++ [1, 2, 3].
