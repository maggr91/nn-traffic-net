-module(simulation).

-export([start/2, run_simulation/1]).


start(Lapse, MaxSpeed) ->
    %%MaxSpeed = 50,    
    register(poissonServer,prob:start({poisson,6})),
    register(geoServer,prob:start({geometrica,0.5})),
    register(geoCedServer,prob:start({geometrica,0.25})),    
    register(trafficServer, traffic:start(MaxSpeed)),
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
    %%timer:apply_after(300, simulation, unregister_process, []). 
stop() -> 
	stop_probServers(),
    call(stop).

%%unregister_process() ->
%%    unregister(poissonServer),
%%    unregister(geoServer),
%%    unregister(geoCedServer),
%%    unregister(trafficServer).

stop_probServers() ->
    poissonServer ! killyou,
    geoServer ! killyou,
    geoCedServer ! killyou.
    
call(Message) ->
    io:format("CALLING PROCESS LOOP.~n",[]),
    trafficServer ! {call, self(), Message},
    receive
       {reply, Reply} -> Reply
    end.
