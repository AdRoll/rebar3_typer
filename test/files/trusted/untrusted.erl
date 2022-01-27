-module(untrusted).

-export([untrusted/0]).

untrusted() ->
    trusted:trusted().
