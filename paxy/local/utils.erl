-module(utils).
-export([get_timeout/0, get_send_strategy/0, get_acceptors/0, get_proposers/0, get_random_color/0]).
-define(TIMEOUT, 2000).
-define(DELAY, 2000).
-define(RED, {255, 0, 0}).
-define(BLUE, {0, 0, 255}).
-define(GREEN, {0, 255, 0}).
-define(NUM_PROP, 3).
-define(NUM_ACC, 5).

get_timeout() ->
    T = os:getenv("timeout"),
    case T of
        false -> ?TIMEOUT;
        _ -> list_to_integer(T)
    end.

get_delay() ->
    D = os:getenv("delay"),
    case D of
        false -> ?DELAY;
        _ -> list_to_integer(D)
    end.

is_dropped() ->
    P = rand:uniform(100),
    D = os:getenv("drop"),
    case D of
        false -> false;
        _ -> P =< list_to_integer(D)
    end.

% normal | {delayed, Delay} | dropped | normal 
get_send_strategy() ->
    case os:getenv("send_mode") of
        "dropped" ->
            case  is_dropped() of
                false -> normal;
                _ -> dropped
            end;
        "delayed" ->
            {delayed, get_delay()};
        _ ->
            normal
    end.

get_unique_id() ->
    Timestamp = os:timestamp(),
    lists:flatten(io_lib:format("~w", [Timestamp])).

get_random_color() ->
    case rand:uniform(3) of
        1 -> ?RED;
        2 -> ?BLUE;
        _ -> ?GREEN
    end.

get_proposers() ->
    Val = os:getenv("proposer"),
    N =
        case Val of
            false -> ?NUM_PROP;
            _ -> Val
        end,
    [get_unique_id() || _ <- lists:seq(1, list_to_integer(N))].

get_acceptors() ->
    Val = os:getenv("acceptor"),
    N =
        case Val of
            false -> ?NUM_ACC;
            _ -> Val
        end,
    [get_unique_id() || _ <- lists:seq(1, list_to_integer(N))].
