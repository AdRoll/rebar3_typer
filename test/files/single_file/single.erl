-module(single).

-export([exported/0]).

exported() ->
    not_exported().

%% @doc Using some external types here, for completeness
not_exported() ->
    lists:foreach(fun(_) -> ignore end, lists:seq(1, 10)).
