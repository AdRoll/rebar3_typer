-module(single).

-export([exported/0]).

exported() ->
    not_exported().

not_exported() ->
    ok.
