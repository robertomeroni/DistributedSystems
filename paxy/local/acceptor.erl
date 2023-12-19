-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    Promised = order:null(),
    Voted = order:null(),
    Value = na,
    acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promised) of
                true ->
                    send(Proposer, {promise, Round, Voted, Value}),
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
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            case order:goe(Round, Promised) of
                true ->
                    send(Proposer, {vote, Round}),

                    case order:goe(Round, Voted) of
                        true ->
                            io:format(
                                "[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                [Name, Promised, Round, Proposal]
                            ),
                            % Update gui

                            % Round
                            PanelId !
                                {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                                    "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
                            acceptor(Name, Promised, Round, Proposal, PanelId);
                        false ->
                            acceptor(Name, Promised, Voted, Value, PanelId)
                    end;
                false ->
                    %maybe optimization: Promised here
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        stop ->
            PanelId ! stop,
            ok
    end.


send(Proposer, Message) ->
    case utils:get_send_strategy() of
        normal -> 
            Proposer ! Message;
        dropped ->
            io:format("message dropped~n");
        {delayed, Delay} -> 
            timer:send_after(rand:uniform(Delay), Proposer, Message)
    end.
