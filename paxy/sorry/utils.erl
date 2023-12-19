-module(utils).
-export([get_delay/0]).
-define(DELAY, 2000).

get_delay() ->
    D = os:getenv("delay"),
    case D of
        false -> ?DELAY;
        _ -> list_to_integer(D)
    end.