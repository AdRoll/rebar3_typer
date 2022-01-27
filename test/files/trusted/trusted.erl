-module(trusted).

-export([trusted/0]).

%% It's a lie!
-spec trusted() -> untrusted.
trusted() ->
    trusted.
