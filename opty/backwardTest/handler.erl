-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->         
    receive
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of  % COMPLETED
                {N, _, Value} ->
                    Client ! {value, Ref, Value}, % ADDED 
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                    Entry = store:lookup(N,Store), % ADDED
                    Entry ! {read, Ref, self()}, % ADDED
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        {Ref, Entry, Value, Time} ->
            Client ! {value, Ref, Value}, % ADDED
            % Remember that the timestamp is assigned at the END of read phase
            handler(Client, Validator, Store, [{Entry, Time}|Reads], Writes); % COMPLETED
        {write, N, Value} ->
            Entry = store:lookup(N, Store),  % ADDED
            Added = lists:keystore(N, 1, Writes, {N, Entry, Value}), % COMPLETED
            handler(Client, Validator, Store, Reads, Added);
        {commit, Ref} ->
            Validator ! {validate, Ref, Reads, Writes, Client}; % ADDED
        abort ->
            ok
    end.