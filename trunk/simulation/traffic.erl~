-module(traffic).

-export([start/1, stop/0, lane/5, set_map/0, connect/1, connect_lanes/1, test/0, write_result/2]).

-export([init/0]).

-export([test_move_av/1,test_move_ca/1, test_idle/1]).

%-import(light_fsm,[init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%client calls
-import(light_fsm,[start_link/0,move_avenue/1, move_street/1, idle/1,update_siblings/2, evaluate_state/1]).


%% This module is used to run the simulation
%% of normal traffic, with no modification of ANN
test() ->
    light_fsm:start_link().
    
test_move_av(LightId) ->
    light_fsm:move_avenue(LightId).
    
test_move_ca(LightId) ->
    light_fsm:move_street(LightId).

test_idle(LightId) ->
    light_fsm:idle(LightId).

start(Lapse) ->
    register(traffic, spawn(traffic, init, [])),
    run_simulation(Lapse, 0).

run_simulation(Lapse, Current) when Current < Lapse ->
    call(continue),   
    run_simulation(Lapse, Current + 1);
run_simulation(Lapse, Current) when Current >= Lapse ->
    stop().
 
stop() -> 
    call(stop).

call(Message) ->
    traffic ! {call, self(), Message},
    receive
       {reply, Reply} -> Reply
    end.

 
init() ->
    MapData = set_map(), 
    loop(MapData, 0).

%% set the network of traffic
set_map() ->
    Axis = get_lights(),
    Roads = get_lanes(),
    {_Origin, AllocatedLights} = allocate_lights({Axis,[]}),
    {_OrigLanes, AllocatedLanes} = allocate_lanes({Roads,[]}),
  
    connect_lanes({AllocatedLanes,AllocatedLanes}),
    SourceLanes = get_source_lanes(AllocatedLanes, []),
    
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/logs/",
    
    io:format("PATH ~s.~n",[Path]),
    
    LightsFSM = connect({AllocatedLights, AllocatedLanes, [],Path}), 
    set_connection_to_light_siblings(LightsFSM,LightsFSM),
    io:format("SourceLanes ~w.~n",[SourceLanes]),
    {LightsFSM, SourceLanes}.
    
    
%% Spawn every intersection and lane on the map
allocate_lights({[],Spawned})->
    {[],Spawned};
allocate_lights({[{LightId,_ManagedLanes, Siblings, Cycle_time, Go_time}|Tail], Spawned}) ->
    %%Pid = spawn(traffic,light, [ManagedLanes,Siblings, Cycle_time, Go_time]),
    allocate_lights({Tail, [{LightId,Siblings, Cycle_time, Go_time} | Spawned]}). 
          
allocate_lanes({[], Spawned}) ->
    {[],Spawned};
allocate_lanes({[{LaneId,LightController,Dir, Type, ConnectedLanes, 
                  CarsQueque, IsSource, Capacity}|Tail], Spawned}) ->
    Pid = spawn(traffic,lane, [Type, [], CarsQueque, Capacity, []]),
    allocate_lanes({Tail, [{LaneId,Pid, LightController,Dir,ConnectedLanes, IsSource} | Spawned]}).

%%connect lights with its lanes
connect({[], _LaneList, LightsFSM,_LogData}) -> 
    LightsFSM;
connect({[{LightId,Siblings, Cycle_time, Go_time} | TailLight], LaneList,LightsFSM, LogData}) -> 
    io:format("Luz: ~w / lineas: ~w.~n",[LightId, LaneList]),
    {ManagedLanes, RemLanes} = find_lanes({LightId, {[],[]}}, LaneList, []),
    io:format("Managed Lanes: ~w.~n~n",[ManagedLanes]),
    PathLog = (LogData ++ lists:flatten(io_lib:format("~p",[LightId]))) ++ ".txt",
    {ok, LightPid} = light_fsm:start_link({LightId, ManagedLanes,Siblings, Cycle_time, Go_time, PathLog}),
    connect({TailLight, RemLanes, [{LightId,LightPid}|LightsFSM], LogData}).

find_lanes({LightId,ManagedLanes}, [], RemLanes) -> 
    io:format("Exit ~w / sin lineas. Rem Lanes: ~w~n~n",[{LightId,ManagedLanes},RemLanes]),
    {ManagedLanes, RemLanes};
find_lanes({LightId,{Av, Ca}}, [{LaneId,LanePid, LightId,Dir,_AdjLanes, _IsSource}| Tail], RemLanes) ->
    io:format("Coincidencia: ~w envio de mensaje.~n",[{LightId,LaneId}]),
    %LightPid ! {connect_lane, LanePid, self(),Dir},
    io:format("{Av ~w, Ca: ~w.~n",[Av, Ca]),
    if
        Dir =:= av -> 
    	    NewManaged = {[{LaneId,LanePid}| Av], Ca},    	    
    	    io:format("light ~w, lanes: ~w.Dir:~w~n",[self(), NewManaged, Dir]),
    	    io:format("Coincidencia: mensaje enviado.~n",[]),
            io:format("Coincidencia: mensaje recibido.~n~n",[]),
            find_lanes({LightId,NewManaged},Tail, RemLanes);
    	Dir =:= ca ->
    	    NewManaged = {Av, [{LaneId,LanePid}| Ca]},
    	    io:format("light ~w, lanes: ~w. Dir:~w~n ",[self(), NewManaged,Dir]),
    	    io:format("Coincidencia: mensaje enviado.~n",[]),
            io:format("Coincidencia: mensaje recibido.~n~n",[]),
            find_lanes({LightId,NewManaged},Tail, RemLanes);
        true ->
     	    io:format("NO TYPE MATCH",[])     	    
    end;
  
find_lanes({LightId, ManagedLanes}, [LHead | LTail], RemLanes) ->
    io:format("No Coincidencia: ~w continue.~n",[{LightId,ManagedLanes}]),
    find_lanes({LightId, ManagedLanes}, LTail, [LHead|RemLanes]).


%%% Connect each lane with other near lanes
connect_lanes({[], _LaneList}) -> 
    {[],{ok, ready}};
connect_lanes({[{LaneId,Pid, _Light,_Dir, AdjLanes, _IsSource} | TailLane], LaneList}) -> 
    io:format("Carril: ~w / lineas adj: ~w.~n",[{LaneId,Pid}, AdjLanes]),
    set_connection_to_lane({LaneId,Pid}, AdjLanes, LaneList),
    connect_lanes({TailLane, LaneList}).
  
set_connection_to_lane({LaneId,Pid}, [], LanesList) -> 
    io:format("Exit ~w / sin lineas. List of Lanes: ~w~n~n",[{LaneId,Pid},LanesList]),
    [];
set_connection_to_lane({LaneId,Pid}, [AdjLane | AdjLanesTail], LaneList) ->
    find_adjLanes({LaneId,Pid},AdjLane,LaneList),
    set_connection_to_lane({LaneId,Pid},AdjLanesTail, LaneList).
  
find_adjLanes({LaneId,LanePid}, AdjLaneId, []) -> 
    io:format("Exit ~w / sin lineas. lineaBuscada: ~w~n~n",[{LaneId,LanePid},AdjLaneId]),
    {[],{ok, nolanes}};
find_adjLanes({LaneId,LanePid}, AdjLaneId, [{AdjLaneId,AdjLanePid, _Light,_Dir, _AdjLanes, _IsSource}|_Tail]) ->
    io:format("Coincidencia: ~w envio de mensaje.~n",[{LaneId,LanePid}]),
    LanePid ! {connect_lane, {AdjLaneId,AdjLanePid}, self()},
    io:format("Coincidencia: mensaje enviado.~n",[]),
    receive
        {reply, error} -> error;
  	{reply, ok} -> 
            io:format("Coincidencia: mensaje recibido. Linea agregada~n~n",[]),
  	    {ok, added}
    end;
  
find_adjLanes({LaneId, LanePid}, AdjLaneId, [_LHead | LTail]) ->
    io:format("No Coincidencia: ~w continue.~n",[{LaneId, LanePid}]),
    find_adjLanes({LaneId, LanePid}, AdjLaneId, LTail).

%% Connect traffic lights with siblings
set_connection_to_light_siblings([], _LightsList) -> [];
set_connection_to_light_siblings([{Id, Pid}|LightsRem], LightsList) ->
    {_State, _Cycle_time, _Go_time, Siblings, _LogData} = light_fsm:get_state(Pid),
%    io:format("Buscando hermanos de: ~w en ~w.~n",[{Id,Pid},Siblings]),
    CompleteSiblings = find_siblings(Siblings,LightsList,[]),
%    io:format("Hermanos encontrados ~w.~n",[CompleteSiblings]),
    
    light_fsm:update_siblings(Pid, CompleteSiblings),
    set_connection_to_light_siblings(LightsRem,LightsList).
    
find_siblings([],_LightsList, CompleteSiblings) ->
    lists:reverse(CompleteSiblings);
find_siblings([SiblingId|Tail], LightsList,CompleteSiblings) ->
    SiblingData = lists:keyfind(SiblingId, 1, LightsList), 
    find_siblings(Tail,LightsList, [SiblingData|CompleteSiblings]).
    

%%text file initialization of map
get_lights() -> 
    {ok, Cwd} = file:get_cwd(),
    %%io:format("Directorio: ",Cwd),
    Path = Cwd ++ "/sources/prueba2.txt",
    readlines(Path).
 
get_lanes() ->
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/sources/prueba.txt",
    readlines(Path).

%% Separete lanes that will function as sources for new cars
get_source_lanes([], SourceLanes) ->
    SourceLanes;
get_source_lanes([{LaneId,Pid, _LightController,_Dir,_ConnectedLanes, source_lane} | Tail], SourceLanes) ->
    get_source_lanes(Tail, [{LaneId,Pid, []} | SourceLanes]);
get_source_lanes([{_LaneId,_Pid, _LightController,_Dir,_ConnectedLanes, _Isource_lane} | Tail], SourceLanes) ->
    get_source_lanes(Tail, SourceLanes).
    
%% Read files to get lanes/ lights config
readlines(FileName) ->
    {ok, Device} =  file:open(FileName, [read]),
    Result = get_all_lines(Device, []),
    lists:map(fun(X) -> 
   	         Stripped = string:strip(X, right, $\n),
   		 {ok, ItemsTokens, _} = erl_scan:string(Stripped ++ "."),
		 {ok, Term} = erl_parse:parse_term(ItemsTokens),
		 Term
   	      end,
   	      Result
   	    ).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        {error, Reason} -> Reason;
        eof  	      -> file:close(Device), lists:reverse (Accum);
        Line 	      -> get_all_lines(Device, [Line|Accum])      
    end.
   
%% Write down the results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).

loop({Intersections, SourceLanes}, Time) ->
  %% On EVERY intersection in the system
  %% get the light_fsm and run each simulation for it
  %% according to the state of the FSM it will or will not move  the cars
  
  receive
      {call, Pid, continue} ->
         io:format("Running simulation iteration: ~w continue. Intersections ~w~n",[Time, Intersections]),
         traverse_network(Intersections, Time),
         UpdatedSources = estimate_new_arrival(SourceLanes, 0,[]),             
	 reply(Pid, {iteration, ok}),
	 io:format("Finishing simulation iteration: ~w continue. Intersections~n",[Time]),
	 io:format("----------------------------------------------------------~n~n"),
	 loop({Intersections, UpdatedSources}, Time + 1);
    {call,Pid, stop} -> 
         reply(Pid, {normal, Time})
  end.

traverse_network([], _Time) ->
    [];
traverse_network([{InterId, Intersection}|Intersections], Time) ->
    io:format("Evaluating state for light_fsm: ~w continue.~n",[InterId]),
    light_fsm:evaluate_state({InterId, Intersection, Time}),
    traverse_network(Intersections, Time).

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

lane(Type, ConnectedLanes, CarsQueque, Capacity, Obstruction) ->
  %% For each lane on every street in the system, 
  %% get the move acording to the ligth state passed 
  %% and run the complete simulation
    receive
        {go, LightController, _TimeCycle, _Time, LogData} -> %% if a go message is received it tells all cars to start to move
            io:format("GO msj~n. CarsQueque: ~w~n",[{Type, ConnectedLanes, CarsQueque, Obstruction}]),
	    NewCarsQueque = move_cars(CarsQueque, []),
	    io:format("Moving msj received from ~w. CarsQueque: ~w ~n",[LightController,NewCarsQueque]),
	    write_result(LogData, io_lib:format("Moving msj received from ~w",[LightController])),
	    reply(LightController, updated),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction);
        {stop, LightController, LogData} ->  
	%% send a message to all cars to start to stop preventing them to pass the red ligth
	    NewCarsQueque = stop_cars(CarsQueque,LightController),
	    io:format("Stop msj received from ~w.~n",[LightController]),
	    write_result(LogData, io_lib:format("Stop msj received from ~w",[LightController])),
	    reply(LightController, updatedStop),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction);
      	{dispatch, LightController, Time, _LogData} ->		
	%% In case that the car reaches the end of the lane it has to be changed to 
	%% a new lane.
	    NewCarsQueque = dispatch_cars(CarsQueque, ConnectedLanes, Time, LightController),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction);
	{waiting, LightController, Time, LogData} ->
%	    io:format("Waiting msj~n. CarsQueque: ~w~n",[{Type, ConnectedLanes, CarsQueque}]),
	    NewCarsQueque = waiting(CarsQueque,[]),
	    io:format("Waiting msj received from ~w. LogData~p~n",[LightController,LogData]),
	    write_result(LogData, io_lib:format("Waiting msj received from ~w",[LightController])),
	    reply(LightController, updatedWaiting),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction);
	{connect_lane, AdjLane, Pid}  ->     		
    	    io:format("{Connected Lanes ~w.~n",[ConnectedLanes]),
    	    NewConnectedLanes = [AdjLane| ConnectedLanes],
     	    io:format("{new Connected Lanes ~w.~n",[NewConnectedLanes]),
    	    reply(Pid,ok),
    	    lane(Type, NewConnectedLanes, CarsQueque, Capacity, Obstruction);
	{incoming, Pid, Car} ->
	    io:format("Car incoming ~w.~n",[Car]),
	    case length(CarsQueque) < Capacity of
	        true  ->
	            NewCarsQueque = lists:reverse([Car|lists:reverse(CarsQueque)]),
	            reply(Pid, ok),
	            lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction);
	        false ->
	            reply(Pid, full),
	            lane(Type, ConnectedLanes, CarsQueque, Capacity, Obstruction)
	    end;
	{transfer, Pid} ->
	    reply(Pid, ok);     	        	
	stop -> {ok, normal}
    end.  

move_cars([], UpdatedCars) -> 
    io:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
move_cars([{car,{Wait,Delay}}|Tail], UpdatedCars) -> 
    io:format("moving cars ~n"),
    move_cars(Tail, [{car,{Wait + 1,Delay}} | UpdatedCars]).
    %%{go, Line1, Line2, Time} -> true.

stop_cars([], _) -> [];
stop_cars([FirstCar|Waiting], Pid) -> true.

dispatch_cars([], _ConnectedLanes, _Time, Pid) -> [];
dispatch_cars([FirstCar|Waiting], ConnectedLanes, Time, Pid) -> true.

waiting([], UpdatedCars) -> 
    io:format("Updated waiting ~w, ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
waiting([{car,{Wait,Delay}}|Tail], UpdatedCars) -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1}} | UpdatedCars]). 

add_car({LaneId, LanePid, WaitingOutside}, Car, SourcesLane) ->    
    LanePid ! {incoming, self(), Car},
    receive
        {reply, full}  -> 
            WaitingUpdated = waiting([Car|WaitingOutside], []),
            [{LaneId, LanePid, WaitingUpdated} | SourcesLane];
        {reply, ok}    -> 
            WaitingUpdated = waiting(WaitingOutside,[]), 
            [{LaneId, LanePid, WaitingUpdated} | SourcesLane]
    end.

%% Estimate arrival for new cars to the zone    
estimate_new_arrival([], _Prob,SourcesLane) ->
    io:format("Ending ~w ~n",[SourcesLane]),
    SourcesLane;
estimate_new_arrival([{LaneId, LanePid, []}| Tail], Prob, SourcesLane) when Prob > 50  ->
    io:format("ADDING CAR TO LANE ~n",[]),
    Car = {car,{0,0}},
    UpdatedSources = add_car({LaneId, LanePid, []}, Car, SourcesLane),
    estimate_new_arrival(Tail,random:uniform(100), UpdatedSources);
estimate_new_arrival([{LaneId, LanePid, [WaitingCar|WaitingOutside]}| Tail], Prob, SourcesLane) when Prob > 50  ->
    io:format("ADDING CAR TO LANE ~n",[]),
    NewWaiting = lists:reverse([{car,{0,0}} | lists:reverse(WaitingOutside)]),
    UpdatedSources = add_car({LaneId, LanePid, NewWaiting}, WaitingCar, SourcesLane),
    estimate_new_arrival(Tail,random:uniform(100), UpdatedSources);
estimate_new_arrival([{LaneId, LanePid, WaitingOutside}| Tail], Prob, SourcesLane) when Prob =< 50  ->
    io:format("NOT ADDING CAR TO LANE ~n",[]),
    WaitingUpdated = waiting(WaitingOutside,[]), 
    estimate_new_arrival(Tail,random:uniform(100), [{LaneId, LanePid, WaitingUpdated}|SourcesLane]).
