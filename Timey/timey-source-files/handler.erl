-module(handler).
-export([start/4]).

start(Client, Tref, Time, Store) ->
    spawn_link(fun() -> init(Client, Tref, Time, Store) end).

init(Client, Tref, Time, Store) ->
    handler(Client, Tref, Time, Store, [], [], [], []).

handler(Client, Tref, Time, Store, UnvalRd, ValRd, UnvalWr, TentativeWr) ->	
    receive
        {read, Ref, N} ->
            Entry = store:lookup(N, Store), 
            Entry ! {read, Ref, Time, self()}, 
            handler(Client, Tref, Time, Store, [{Ref, Entry}|UnvalRd], ValRd, UnvalWr, TentativeWr);

        {replyrd, {ok, Value}, Ref} ->
            {value, {Ref, Entry}, NewUnvalRd} = lists:keytake(Ref, 1, UnvalRd),
            Client ! {value, Ref, {ok, Value}},
            NewValRd = lists:keystore(Entry, 1, ValRd, {Entry}),
            handler(Client, Tref, Time, Store, NewUnvalRd, NewValRd, UnvalWr, TentativeWr);

        {write, Ref, N, Value} ->
            Entry = store:lookup(N, Store),
            Entry ! {write, Ref, Time, Value, self()},
            handler(Client, Tref, Time, Store, UnvalRd, ValRd, [{Ref, Entry}|UnvalWr], TentativeWr);

        {replywr, ok, Ref} ->
            {value, {Ref, Entry}, NewUnvalWr} = lists:keytake(Ref, 1, UnvalWr),
            NewTentativeWr = lists:keystore(Entry, 1, TentativeWr, {Entry}),
            Client ! {value, Ref, ok},  
            handler(Client, Tref, Time, Store, UnvalRd, ValRd, NewUnvalWr, NewTentativeWr);

        {reply, abort, _Ref} ->
            lists:foreach(fun({Pid}) -> Pid ! {abortrd, Time} end, ValRd),
            lists:foreach(fun({_, Pid}) -> Pid ! {abortwr, Time} end, UnvalWr),
            lists:foreach(fun({Pid}) -> Pid ! {abortwr, Time} end, TentativeWr),
            Client ! {abort, Tref};

        commit ->
            lists:foreach(fun({_, Pid}) -> Pid ! {commit, Time} end, UnvalWr),
            lists:foreach(fun({Pid}) -> Pid ! {commit, Time} end, TentativeWr),
            case pending_val(Client, Tref, UnvalRd, UnvalWr) of
                ok ->
                    Client ! {commit, Tref};
                abort ->
                    Client ! {abort, Tref}
            end;

        abort ->
            lists:foreach(fun({Pid}) -> Pid ! {abortrd, Time} end, ValRd),
            lists:foreach(fun({_, Pid}) -> Pid ! {abortwr, Time} end, UnvalWr),
            lists:foreach(fun({Pid}) -> Pid ! {abortwr, Time} end, TentativeWr)
    end.

pending_val(_, _, [], []) ->
    ok;
pending_val(Client, Tref, UnvalRd, UnvalWr) ->
    receive
        {reply, abort, _Ref} ->
            abort;
        {replyrd, {ok, Value}, Ref} ->
            Client ! {value, Ref, {ok, Value}},
            NewUnvalRd = lists:keydelete(Ref, 1, UnvalRd),
            pending_val(Client, Tref, NewUnvalRd, UnvalWr);
        {replywr, ok, Ref} ->
            Client ! {value, Ref, ok},
            NewUnvalWr = lists:keydelete(Ref, 1, UnvalWr),
            pending_val(Client, Tref, UnvalRd, NewUnvalWr)
    end.
