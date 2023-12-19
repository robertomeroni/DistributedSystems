-module(paxy).
-export([start/1,stop/0, stop/1,crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).


start(Sleep) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  AccRegister = [homer, marge, bart, lisa, maggie],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        done(AccRegister),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

done(Acceptors) ->
    Fun = fun(Acceptor) ->
        proposer:send(Acceptor,done)
    end,
    lists:foreach(Fun, Acceptors).

stop() ->
  stop(homer),
  stop(marge),
  stop(bart),
  stop(lisa),
  stop(maggie),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.

crash(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            io:format("[Acceptor] ~w CRASHED~n", [Name]),
            pers:open(Name),
            {_, _, _, Pn} = pers:read(Name),
            Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
            pers:close(Name),
            unregister(Name),
            exit(Pid, "crash"),
            timer:sleep(3000),
            register(Name, acceptor:start(Name, na))
    end.
