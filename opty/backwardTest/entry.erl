-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            From ! {Ref, self() ,Value, Time}, % ADDED
            entry(Value, Time);
        {write, New} ->
            entry(New , make_ref());  % COMPLETED
        {check, Ref, Readtime, From} ->
            if 
                Readtime == Time ->   % COMPLETED
                    %% ADDED
                    From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.