-module(acceptor).
-export([start/2]).
-define(delay_promise, 2000).
-define(delay_accept, 2000).
-define(drop, 1).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    pers:open(Name),
    % init/2 is called ONLY when acceptor is created or restarted
    % - creation: PanelId has correct value and Pn = na
    % - restart: Pn has correct value and PanelId = na
    {Promised, Voted, Value, Pn} = pers:read(Name),

    %Value = na for the read on empty storage
    Colour =
        case Value of
            na -> {0, 0, 0};
            _ -> Value
        end,
    Pn_id =
        case Pn of
            % first time acceptor is created
            na ->
                io:format("[Acceptor ~w] Phase 1: SETUP PanelId. Store PanelId in the backup.~n", [
                    Name
                ]),
                pers:store(Name, Promised, Voted, Colour, PanelId),
                PanelId;
            % acceptor is restarted
            _ ->
                io:format("[Acceptor ~w] RESTART with Promise: ~w, Voted: ~w, Colour: ~w ~n", [
                    Name,Promised, Voted, Colour
                ]),
                Pn
        end,
    Pn_id !
        {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
            "Promised: " ++ io_lib:format("~p", [Promised]), Colour},
    acceptor(Name, Promised, Voted, Colour, Pn_id).

get_delay() ->
    D = os:getenv("delay"),
    case D of
        false -> ?delay_promise;
        _ -> list_to_integer(D)
    end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promised) of
                true ->
                    pers:store(Name, Round, Voted, Value, PanelId),

                    T = rand:uniform(get_delay()),

                    timer:send_after(T,Proposer,{promise, Round, Voted, Value}),
                    io:format(
                        "[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                        [Name, Round, Voted, Value]
                    ),
                    % Update gui
                    Colour =
                        case Value of
                            na -> {0, 0, 0};
                            _ -> Value
                        end,
                    PanelId !
                        {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                            "Promised: " ++ io_lib:format("~p", [Round]), Colour},
                    acceptor(Name, Round, Voted, Value, PanelId);
                false ->
                    Proposer ! {sorry, {prepare, Round}},

                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            case order:goe(Round, Promised) of
                true ->
                    case order:goe(Round, Voted) of
                        true ->
                            pers:store(Name, Promised, Round, Proposal, PanelId),
                            Proposer ! {vote, Round},
                            io:format(
                                "[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                [Name, Promised, Round, Proposal]
                            ),

                            % Update gui
                            PanelId !
                                {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                                    "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
                            acceptor(Name, Promised, Round, Proposal, PanelId);
                        false ->
                            Proposer ! {vote, Round},
                            acceptor(Name, Promised, Voted, Value, PanelId)
                    end;
                false ->
                    Proposer ! {sorry, {accept, Round}},
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        stop ->
            PanelId ! stop,
            io:format("[Acceptor ~w] Phase 2: STOPPED successfully. It DELETES the backup.~n", [Name]),
            pers:close(Name),
            pers:delete(Name),
            ok;
        done ->
            io:format("[Acceptor ~w] Phase 2: FINISHED successfully. It DELETES the backup.~n", [Name]),
            pers:close(Name),
            pers:delete(Name)
    end.
