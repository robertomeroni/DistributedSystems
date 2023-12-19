-module(opty).
-export([start/6, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%% SubsetSize: Number of entries assigned to each client

get_subset(Entries, SubsetSize) ->
    lists:sublist(
        [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])],
        SubsetSize
    ).

start(Clients, Entries, Reads, Writes, Time, SubsetSize) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Reads, Writes, SubsetSize),
    io:format(
        "Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~ws, SUBSET_SIZE ~w~n",
        [Clients, Entries, Reads, Writes, Time, SubsetSize]
    ),
    timer:sleep(Time * 1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _, _) ->
    L;
startClients(Clients, L, Entries, Reads, Writes, SubsetSize) ->
    EntrySubset = get_subset(Entries, SubsetSize),
    % Be careful that now EntrySubset is a list of assigned entries for a client
    % instead of a integer expressing the total number of entries 
    Pid = client:start(Clients, EntrySubset, Reads, Writes, s),
    startClients(Clients - 1, [Pid | L], Entries, Reads, Writes, SubsetSize).

stopClients([]) ->
    ok;
stopClients([Pid | L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.
