-module(traffic).

-export([start/1, stop/0, lane/6, set_map/0, set_lanes_map/0, connect/1, connect_lanes/1, test/0, write_result/2]).

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
  
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/logs/",
    io:format("PATH ~s.~n",[Path]),
    NewPath = Path ++ "arrival_log.txt",
    
    connect_lanes({AllocatedLanes,AllocatedLanes}),
    SourceLanes = get_source_lanes(AllocatedLanes),
    
    LightsFSM = connect({AllocatedLights, AllocatedLanes, [],Path}), 
    set_connection_to_light_siblings(LightsFSM,LightsFSM),
    io:format("SourceLanes ~w.~n",[SourceLanes]),
    {LightsFSM, SourceLanes, NewPath}.

set_lanes_map() ->    
    Roads = get_lanes(),
    {_OrigLanes, AllocatedLanes} = allocate_lanes({Roads,[]}),  
    connect_lanes({AllocatedLanes,AllocatedLanes}).
    
%% Spawn every intersection and lane on the map
allocate_lights({[],Spawned})->
    {[],Spawned};
allocate_lights({[{LightId,_ManagedLanes, Siblings, Cycle_time, Go_time}|Tail], Spawned}) ->
    %%Pid = spawn(traffic,light, [ManagedLanes,Siblings, Cycle_time, Go_time]),
    allocate_lights({Tail, [{LightId,Siblings, Cycle_time, Go_time} | Spawned]}). 
          
allocate_lanes({[], Spawned}) ->
    {[],Spawned};
allocate_lanes({[{LaneId,LightController,Dir, Type, ConnectedLanes, 
                  CarsQueque, IsSource, Capacity, {probList, ProbData}}|Tail], Spawned}) ->
    Pid = spawn(traffic,lane, [Type, [], CarsQueque, Capacity, [], ProbData]),
    {arrival, ProbList} = lists:keyfind(arrival, 1, ProbData),
    ProbRanges = list_to_tuple(ProbList),
    allocate_lanes({Tail, [{LaneId,Pid, LightController,Dir,ConnectedLanes, IsSource, ProbRanges} | Spawned]}).

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
find_lanes({LightId,{Av, Ca}}, [{LaneId,LanePid, LightId,Dir,_AdjLanes, _IsSource, _ProbData}| Tail], RemLanes) ->
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
connect_lanes({[{LaneId,Pid, _Light,_Dir, AdjLanes, _IsSource, _ProbData} | TailLane], LaneList}) -> 
    io:format("Carril: ~w / lineas adj: ~w.~n",[{LaneId,Pid}, AdjLanes]),
    set_connection_to_lane({LaneId,Pid}, AdjLanes, LaneList),
    connect_lanes({TailLane, LaneList}).
  
set_connection_to_lane({LaneId,LanePid}, [], _LanesList) -> 
    io:format("Exit ~w / sin lineas.~n~n",[{LaneId,LanePid}]),
    [];
set_connection_to_lane({LaneId,LanePid}, [{Type, AdjLaneTypeList} | AdjLanesTypeTail], LaneList) ->    
    AdjLaneTypeData = iterate_Adj_lane_types(AdjLaneTypeList,LaneList,[]),
    io:format("Coincidencia: ~w envio de mensaje.~n",[{LaneId,LanePid}]),
    LanePid ! {connect_lane, {Type, AdjLaneTypeData}, self()},
    io:format("Coincidencia: mensaje enviado.~n",[]),
    receive
        {reply, error} -> error;
  	{reply, ok} -> 
            io:format("Coincidencia: mensaje recibido. Linea agregada~n~n",[]),
            set_connection_to_lane({LaneId,LanePid},AdjLanesTypeTail, LaneList)
    end.   
    
iterate_Adj_lane_types([],_LaneList, TypeAdjList) -> lists:reverse(TypeAdjList);
iterate_Adj_lane_types([AdjLane | AdjLanesTail],LaneList, TypeAdjList) ->
    AdjLaneData = find_adjLanes(AdjLane,LaneList),
    iterate_Adj_lane_types(AdjLanesTail, LaneList, [AdjLaneData | TypeAdjList]).    
    
  
find_adjLanes(AdjLaneId, []) -> 
    io:format("No Coincidencia: ~w continue.~n",[AdjLaneId]),
    {};
find_adjLanes(AdjLaneId, [{AdjLaneId,AdjLanePid, _Light,_Dir, _AdjLanes, _IsSource, _ProbData}|_Tail]) ->
    {AdjLaneId,AdjLanePid};  
find_adjLanes(AdjLaneId, [_LHead | LTail]) ->
    find_adjLanes(AdjLaneId, LTail).

%% Connect traffic lights with siblings
set_connection_to_light_siblings([], _LightsList) -> [];
set_connection_to_light_siblings([{_Id, Pid}|LightsRem], LightsList) ->
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

%get_statistics() ->
%    {ok, Cwd} = file:get_cwd(),
%    Path = Cwd ++ "/sources/prob.txt",
%    readlines(Path).

%% Separete lanes that will function as sources for new cars
get_source_lanes(AllocatedLanes) ->    
    get_source_lanes(AllocatedLanes, []).
get_source_lanes([], SourceLanes) ->
    SourceLanes;
get_source_lanes([{LaneId,Pid, _LightController,_Dir,_ConnectedLanes, source_lane, ProbData} | Tail], SourceLanes) ->
    get_source_lanes(Tail, [{LaneId,Pid, [], ProbData} | SourceLanes]);
get_source_lanes([{_LaneId,_Pid, _LightController,_Dir,_ConnectedLanes, _Isource_lane, _ProbData} | Tail], SourceLanes) ->
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

loop({Intersections, SourceLanes, LogPath}, Time) ->
  %% On EVERY intersection in the system
  %% get the light_fsm and run each simulation for it
  %% according to the state of the FSM it will or will not move  the cars
  
  receive
      {call, Pid, continue} ->
         io:format("Running simulation iteration: ~w continue. Intersections ~w~n",[Time, Intersections]),
         traverse_network(Intersections, Time),
         UpdatedSources = estimate_new_arrival(SourceLanes, LogPath),             
	 reply(Pid, {iteration, ok}),
	 io:format("Finishing simulation iteration: ~w continue. Intersections~n",[Time]),
	 io:format("----------------------------------------------------------~n~n"),
	 loop({Intersections, UpdatedSources, LogPath}, Time + 1);
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

lane(Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData) ->
  %% For each lane on every street in the system, 
  %% get the move acording to the ligth state passed 
  %% and run the complete simulation
    receive
        {go, LightController, _TimeCycle, _Time, LogData} -> %% if a go message is received it tells all cars to start to move
            io:format("GO msj~n. CarsQueque: ~w~n",[{Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}]),
            %%{probList, Data} = ProbData,
            %%io:format("probdata: ~w~n",[Data]),
	    NewCarsQueque = move_cars(CarsQueque, ConnectedLanes,Obstruction, [], Capacity,ProbData),
	    io:format("Moving msj received from ~w. CarsQueque: ~w ~n",[LightController,NewCarsQueque]),
	    write_result(LogData, io_lib:format("Moving msj received from ~w",[LightController])),
	    reply(LightController, updated),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
        {stop, LightController, LogData} ->  
	%% send a message to all cars to start to stop preventing them to pass the red ligth
	    NewCarsQueque = stop_moving(CarsQueque),
	    io:format("Stop msj received from ~w.~n",[LightController]),
	    write_result(LogData, io_lib:format("Stop msj received from ~w",[LightController])),
	    reply(LightController, updatedStop),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
      	{dispatch, LightController, Time, _LogData} ->		
	%% In case that the car reaches the end of the lane it has to be changed to 
	%% a new lane.
	    NewCarsQueque = dispatch_cars(CarsQueque, ConnectedLanes, Time, LightController),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
	{waiting, LightController, _Time, LogData} ->
%	    io:format("Waiting msj~n. CarsQueque: ~w~n",[{Type, ConnectedLanes, CarsQueque}]),
	    NewCarsQueque = waiting(CarsQueque),
	    io:format("Waiting msj received from ~w. LogData~p~n",[LightController,LogData]),
	    write_result(LogData, io_lib:format("Waiting msj received from ~w",[LightController])),
	    reply(LightController, updatedWaiting),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
	{connect_lane, AdjLane, Pid}  ->     		
    	    io:format("{Connected Lanes ~w.~n",[ConnectedLanes]),
    	    NewConnectedLanes = [AdjLane| ConnectedLanes],
     	    io:format("{new Connected Lanes ~w.~n",[NewConnectedLanes]),
    	    reply(Pid,ok),
    	    lane(Type, NewConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData);
	{incoming, Pid, {car, {Wait, Delay, _Position}}} ->
	    io:format("Car incoming~n"),
	    NewCarData = {car,{Wait,Delay,Capacity - 1}},
	    {car, {_LastWait, _LastDelay, LastPosition}} = get_lastPosition(CarsQueque),
    	    io:format("Car incoming ~w. LastCar Position: ~w~n",[NewCarData, LastPosition]),
	    case ((length(CarsQueque) < Capacity) and (LastPosition < Capacity - 1))  of
	        true  ->	            
	            NewCarsQueque = lists:reverse([NewCarData|lists:reverse(CarsQueque)]),
	            reply(Pid, ok),
	            lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
	        false ->
	            reply(Pid, full),
	            lane(Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData)
	    end;
	%%{transfer, Pid, ExtraIncoming} ->
	%%    io:format("OutSchedule of cars incoming"),
	%%    case length(ExtraIncoming) < Capacity of
	%%        true  ->
	            %% Sacar probabilidad de que le den pasada al otro carril
	            %% si no le dan pasada, dejarlo en el mismo carril si cambiar
	            %% posiciones y aumentar contadores
	%%            NewCarsQueque = transfer_at(Car,ConnectedLanes),
	%%            reply(Pid, ok),
	%%            lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction)
	%%    end;
	stop -> {ok, normal}
    end.  

get_lastPosition([]) -> {car, {0,0,-1}};
get_lastPosition(CarsQueque) -> lists:last(CarsQueque).

dispatch_cars([], _ConnectedLanes, _Time, _Pid) -> [];
dispatch_cars([_FirstCar|_Waiting], _ConnectedLanes, _Time, _Pid) -> true.

waiting([]) ->
    [];
waiting([{car,{Wait,Delay,Position}}|Tail]) -> 
    waiting([{car,{Wait,Delay,Position}}|Tail], [], Position).
waiting([], UpdatedCars, _LastPosition) -> 
    io:format("Updated waiting ~w, ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
waiting([{car,{Wait,Delay,Position}}|Tail], UpdatedCars, _LastPosition) when Position == -1-> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position}} | UpdatedCars], Position);  
waiting([{car,{Wait,Delay,Position}}|Tail], UpdatedCars, LastPosition) when Position - 1 >= 0, Position - 1 /= LastPosition -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position - 1}} | UpdatedCars], Position - 1);
waiting([{car,{Wait,Delay,Position}}|Tail], UpdatedCars, LastPosition) when Position == 0; Position - 1 == LastPosition -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position}} | UpdatedCars], Position).  

move_cars([], _ConnectedLanes, _Obstruction, UpdatedCars, _LanCap, _ProbData) -> 
    io:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
move_cars([{car,{Wait,Delay, Position}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, ProbData) when Position - 1 >= 0 -> 
    %%io:format("moving cars ~n"),
    %%function to move cars
    move_cars(Tail, ConnectedLanes,[], [{car,{Wait + 1,Delay, Position - 1}} | UpdatedCars], LanCap, ProbData);
    %%{go, Line1, Line2, Time} -> true.
move_cars([{car,{Wait,Delay, Position}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, ProbData) when Position - 1 < 0 -> 
    %%Dispatch Cars
    Prob = random:uniform(),
    {dispatch, ProbList} = lists:keyfind(dispatch, 1, ProbData),
    ProbRanges = list_to_tuple(ProbList),
    io:format("Prob dispatch: ~w.~n",[Prob]),
    Res = prepare_car_dispatch({car,{Wait,Delay, Position}}, ConnectedLanes, Prob, Tail,ProbRanges),
    io:format("Dispatch result: ~w.~n",[Res]),
    case Res of
        {reply, transfered}        -> move_cars(Tail, ConnectedLanes, [], UpdatedCars, LanCap,ProbData);
        {reply, error, NewUpdated} -> stop_moving(NewUpdated, Position)
    end;
move_cars(CarsQueque, ConnectedLanes, 
    		[ObsData | _Obstruction], _UpdatedCars, _LanCap, _ProbData)-> 
    %%function to move cars to other lanes
    %% Sacar probabilidad de que le den pasada al otro carril
    %% si no le dan pasada, dejarlo en el mismo carril si cambiar
    %% posiciones y aumentar contadores
    %%transfer_cars(CarsQueque, ConnectedLanes, Obstruction)
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
    UpdatedCarsQueque = transfer_enabled(List, length(List), CarsQueque, ObsData),
    UpdatedCarsQueque.

%%Get the probability that the car goes either straight or turn in the corner
%%This calls car_dispatch 
prepare_car_dispatch(Car, ConnectedLanes, Prob, CarsQueque,{Fprob, Mprob, _Eprob}) when Prob >= Fprob, Prob < Mprob->
    {Type, List} = lists:nth(1, ConnectedLanes),
    io:format("Prepare dispatch to lane: ~w.~n",[{Type, List}]),
    car_dispatch(Car, List, CarsQueque, self());
prepare_car_dispatch(Car, ConnectedLanes, Prob, CarsQueque, {_Fprob, Mprob, Eprob}) when Prob >= Mprob, Prob =< Eprob->
    {Type, List} = lists:nth(2, ConnectedLanes),
    io:format("Prepare dispatch to lane: ~w.~n",[{Type, List}]),
    car_dispatch(Car, List, CarsQueque, self()).
%prepare_car_dispatch(Car, ConnectedLanes, Prob, CarsQueque) when Prob >= 0.8, Prob =< 0.9->
%   Lane = lists:nth(3, ConnectedLanes),
%    io:format("Prepare dispatch to lane: ~w.~n",[Lane]),
%    car_dispatch(Car, Lane,CarsQueque).

car_dispatch(Car, [], CarsQueque, _CLanePid) ->
    io:format("Nolane. Adding to wait list~n",[]),
    {reply, error, [Car|CarsQueque]};
car_dispatch(Car, [{_LaneId, LanePid} | _Tail], _CarsQueque, CLanePid) when LanePid == CLanePid ->
    io:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, Car]),
    {reply, transfered};

car_dispatch(Car, [{LaneId, LanePid} | _Tail], CarsQueque, CLanePid) when LanePid /= CLanePid ->
%%%%FALTA
    io:format("Lane ~w to dispatch ~w. Car ~w ~n",[self(), {LaneId, LanePid}, Car]),
    LanePid ! {incoming, self(), Car},
    receive
        {reply, full}  ->
            io:format("Lane ~w its full. Adding to wait list~n",[LaneId]),
	    {reply, error, [Car|CarsQueque]};
            %%CarsQuequeUpdated = waiting([Car|CarsQueque], []);
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            {reply, transfered}
            %%CarsQuequeUpdated = waiting(CarsQueque,[]), 
            
    end.
    

%% si hay obstruccion tratar de hacer el paso a alguno de los carriles hermanos y luego de eso
%% obtener la lista actualizada de los carros que quedan y mover el resto hacia adelante

transfer_enabled(List, SiblingsNum, CarsQueque, {_Obs, _Begin, End}) when SiblingsNum == 1 ->
    [Sibling | _Tail] = List,
    attemp_transfer(Sibling, CarsQueque, [], End);
transfer_enabled(List, SiblingsNum, CarsQueque, {_Obs, _Begin, End}) when SiblingsNum /= 1 ->
    [Sibling | _Tail] = lists:reverse(List),
    attemp_transfer(Sibling, CarsQueque, [], End).
    
attemp_transfer(_Sibling, [], UpdatedCars, _ObsPosition) ->
    lists:reverse(UpdatedCars);
attemp_transfer(Sibling, [{car,{Wait,Delay, Position}} | Tail], UpdatedCars, ObsPosition) when Position -1 == ObsPosition ->
    %% When they reached the obstacule stop the cars on the lane and just update times
    Sibling ! {try_transfer, {car,{Wait,Delay, Position}}},
    receive
        ok     -> move_on_transfer_succ(Tail,Position);
        error  -> stop_moving(Tail, Position, [ {car,{Wait + 1,Delay + 1, Position}} | UpdatedCars])
    end;
attemp_transfer(Sibling, [{car,{Wait,Delay, Position}} | Tail], UpdatedCars, ObsPosition) when Position -1 /= ObsPosition ->
    %% when they haven't reached the obstacule try to pass the car to the sibling lane if possible pass if not continue moving
    %% until it reaches the obstacle
    Sibling ! {try_transfer, {car,{Wait,Delay, Position}}},
    receive
        ok     -> attemp_transfer(Sibling, Tail, UpdatedCars, ObsPosition);
        error  -> attemp_transfer(Sibling, [{car,{Wait + 1,Delay, Position - 1}} | Tail], UpdatedCars, ObsPosition)
    end.

%%move cars on transfer success
move_on_transfer_succ(CarsQueque, LastPosition) ->
    move_on_transfer_succ(CarsQueque, LastPosition, []).
move_on_transfer_succ([], _LastPosition, UpdatedCars) ->
    lists:reverse(UpdatedCars);
move_on_transfer_succ([{car,{Wait,Delay, Position}} | Tail], LastPosition, UpdatedCars) when Position >= LastPosition ->
    move_on_transfer_succ(Tail, LastPosition, [{car,{Wait,Delay, Position}} | UpdatedCars]).

%%Stop cars from moving after an obstacle has been found or when the red light has been given
stop_moving([{car,{Wait,Delay, Position}}|Waiting]) ->
    stop_moving(Waiting, Position, [{car,{Wait,Delay, Position}} | []]).
stop_moving(CarsQueque, LastPosition) ->
    stop_moving(CarsQueque, LastPosition, []).
stop_moving([], _LastPosition, UpdatedCars) -> 
    lists:reverse(UpdatedCars);
stop_moving([{car,{Wait,Delay, Position}}|Waiting], LastPosition, UpdatedCars) when LastPosition < Position -1 ->
    stop_moving( Waiting, Position - 1, [{car,{Wait + 1,Delay, Position - 1}} | UpdatedCars]);
stop_moving([{car,{Wait,Delay, Position}}|Waiting], LastPosition, UpdatedCars) when LastPosition >= Position -1 ->
    stop_moving( Waiting, Position, [{car,{Wait + 1,Delay + 1, Position}} | UpdatedCars]).
      
%%stop_cars([], _) -> [];
%%stop_cars([{car,{Wait,Delay, Position}}|Waiting], Pid) -> 
%%    stop_cars([].

add_car({LaneId, LanePid, WaitingOutside, ProbData}, Car, SourcesLane) ->    
    LanePid ! {incoming, self(), Car},
    receive
        {reply, full}  ->
            io:format("Lane ~w its full. Adding to wait list~n",[LaneId]), 
            WaitingUpdated = waiting([Car|WaitingOutside]),
            [{LaneId, LanePid, WaitingUpdated, ProbData} | SourcesLane];
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),  
            WaitingUpdated = waiting(WaitingOutside), 
            [{LaneId, LanePid, WaitingUpdated, ProbData} | SourcesLane]
    end.

%% Estimate arrival for new cars to the zone
estimate_new_arrival(SourcesLane, LogPath) ->
    Prob = random:uniform(),
    estimate_new_arrival(SourcesLane, Prob,[], LogPath).
estimate_new_arrival([], _Prob,SourcesLane, _LogPath) ->
    io:format("Ending ~w ~n",[SourcesLane]),
    SourcesLane;
estimate_new_arrival([{LaneId, LanePid, [], {Fprob, Mprob, Eprob}}| Tail], Prob, SourcesLane, LogPath) when Prob >= Mprob, Prob =< Eprob  ->
    io:format("ADDING CAR TO LANE FROM EMPTY OUTSIDE SOURCE ~n",[]),
    Car = {car,{0,0,-1}},
    UpdatedSources = add_car({LaneId, LanePid, [], {Fprob, Mprob, Eprob}}, Car, SourcesLane),
    estimate_new_arrival(Tail,random:uniform(), UpdatedSources, LogPath);
estimate_new_arrival([{LaneId, LanePid, [WaitingCar|WaitingOutside], {Fprob, Mprob, Eprob}}| Tail], Prob, SourcesLane, LogPath) when Prob >= Mprob, Prob =< Eprob  ->
    io:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE ~n",[]),
    NewWaiting = lists:reverse([{car,{0,0,-1}} | lists:reverse(WaitingOutside)]),
    UpdatedSources = add_car({LaneId, LanePid, NewWaiting, {Fprob, Mprob, Eprob}}, WaitingCar, SourcesLane),
    estimate_new_arrival(Tail,random:uniform(), UpdatedSources, LogPath);
estimate_new_arrival([{LaneId, LanePid, WaitingOutside, {Fprob, Mprob, Eprob}}| Tail], Prob, SourcesLane, LogPath) when Prob >= Fprob, Prob < Mprob  ->
    io:format("NOT ADDING CAR TO LANE ~n",[]),
    WaitingUpdated = waiting(WaitingOutside), 
    estimate_new_arrival(Tail,random:uniform(), [{LaneId, LanePid, WaitingUpdated, {Fprob, Mprob, Eprob}}|SourcesLane], LogPath).
