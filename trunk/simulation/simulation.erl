-module(simulation).

-export([start/1, run_simulation/1]).


start(Lapse) ->    
    register(poissonServer,prob:start({poisson,6})),    
    register(traffic, traffic:start()),
    timer:apply_after(200, simulation, run_simulation, [Lapse]).
    %%run_simulation(Lapse).
    
%%run aux
run_simulation(Lapse) ->    
    run_simulation(Lapse, 0).

%% Loop used to run each iteration of the simulation
run_simulation(Lapse, Current) when Current < Lapse ->
    io:format("CONTINUING with run.~n",[]),
    call(continue),   
    run_simulation(Lapse, Current + 1);
run_simulation(Lapse, Current) when Current >= Lapse ->
    stop().
 
stop() -> 
    call(stop).

call(Message) ->
    io:format("CALLING PROCESS LOOP.~n",[]),
    traffic ! {call, self(), Message},
    receive
       {reply, Reply} -> Reply
    end.
