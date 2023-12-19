-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client, Handler} ->
            block(Writes, Handler),
            Tag = make_ref(),
            send_write_checks(Writes, Tag, Handler),
            case check_writes(length(Writes), Tag) of
                ok ->
                    update(Writes),
                    clean_reads(Reads, Handler),
                    Client ! {Ref, ok};
                abort ->
                    clean_reads(Reads, Handler),
                    Client ! {Ref, abort}
            end,
            unblock(Writes, Handler),
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.

clean_reads(Reads, Handler) ->
    lists:foreach(
        fun(Entry) ->
            Entry ! {clean, Handler}
        end,
        Reads
    ).

block(Writes, Handler) ->
    lists:foreach(
        fun({_, Entry, _}) ->
            Entry ! {block, Handler}
        end,
        Writes
    ).

unblock(Writes, Handler) ->
    lists:foreach(
        fun({_, Entry, _}) ->
            Entry ! {unblock, Handler}
        end,
        Writes
    ).

update(Writes) ->
    lists:foreach(
        fun({_, Entry, Value}) ->
            Entry ! {write, Value}
        end,
        Writes
    ).

send_write_checks(Writes, Tag, Handler) ->
    Self = self(),
    lists:foreach(
        fun({_, Entry, _}) ->
            Entry ! {check, Tag, Self, Handler}
        end,
        Writes
    ).

check_writes(0, _) ->
    ok;
check_writes(N, Tag) ->
    receive
        {Tag, ok} ->
            check_writes(N - 1, Tag);
        {Tag, abort} ->
            abort
    end.
