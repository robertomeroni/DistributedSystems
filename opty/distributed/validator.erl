-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    io:format("Validator started in node ~w ~n", [node()]),
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            % There is only one client in write phase at the same time
            % critical section
            Tag = make_ref(),
            send_read_checks(Reads, Tag),  % COMPLETED
            case check_reads(length(Reads), Tag) of  % COMPLETED
                ok ->
                    update(Writes),  % COMPLETED
                    Client ! {Ref, ok};
                abort ->
                    Client ! {Ref, abort} % ADDED
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.
    
update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
                    Entry ! {write, Value} % ADDED
                  end, 
                  Writes).

send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(fun({Entry, Time}) -> 
                    Entry ! {check, Tag, Time, Self } % ADDED
                  end, 
                  Reads).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            abort
    end.