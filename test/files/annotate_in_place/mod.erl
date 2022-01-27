-module(mod).

-export([exported/0]).

-include("inc.hrl").

exported() ->
    {not_exported(), included()}.

not_exported() ->
    not_exported.
