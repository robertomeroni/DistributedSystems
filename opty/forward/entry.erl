-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, [], ok).

entry(Value, ActiveReads, Owner) ->
    receive
        {block, NewOwner} when Owner == ok ->
            entry(Value, ActiveReads, NewOwner);
        {unblock, Owner} when not (Owner == ok) ->
            entry(Value, ActiveReads, ok);
        {read, Ref, Handler} when Owner == ok ->
            NewList =
                case lists:member(Handler, ActiveReads) of
                    true ->
                        ActiveReads;
                    false ->
                        [Handler | ActiveReads]
                end,
            Handler ! {Ref, self(), Value},
            entry(Value, NewList, Owner);
        {write, New} when Owner == ok ->
            entry(New, ActiveReads, Owner);
        {clean, Handler} ->
            entry(Value, lists:delete(Handler, ActiveReads), Owner);
        {check, Ref, Validator, Handler} ->
            case ActiveReads of
                [Handler] -> Validator ! {Ref, ok};
                [] -> Validator ! {Ref, ok};
                _ -> Validator ! {Ref, abort}
            end,
            entry(Value, ActiveReads, Owner);
        stop ->
            ok
    end.
