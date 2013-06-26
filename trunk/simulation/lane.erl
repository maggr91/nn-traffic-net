-module(lane).

-export([start/1, estimate_new_arrival/2, init_poisson/1, init_geometric/1]).

-export([init/1]).

%% Main call function to spawn proccess
start(Args) ->
    spawn(lane,init, [Args]).
    
init({LaneId, Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData}) ->    
    lane(LaneId, Type, ConnectedLanes, CarsQueque,[], Capacity, Obstruction, ProbData).

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

lane(LaneId, Type, ConnectedLanes, CarsQueque, OutSideArea, Capacity, Obstruction, ProbData) ->
  %% For each lane on every street in the system, 
  %% get the move acording to the ligth state passed on messages
  %% and run the complete simulation
    receive
    %% if a go message is received it tells all cars to start to move
        {go, LightController, _TimeCycle, _Time, LogData} ->
            io:format("GO msj~n. CarsQueque LaneId ~w: ~w~n",[LaneId, {Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}]),
            write_result(LogData, io_lib:format("GO msj~n. CarsQueque LaneId ~w: ~w~n",[LaneId, {Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}])),            
	    {NewCarsQueque, NewProbData, NewOutArea} = move_cars(CarsQueque, ConnectedLanes,Obstruction, [], Capacity,ProbData, LaneId, OutSideArea, LogData),
            io:format("Moving msj received from ~w. NewCarsQueque: ~w ~n",[LightController,NewCarsQueque]),	    
	    reply(LightController, updated),
	    lane(LaneId, Type, ConnectedLanes, NewCarsQueque,NewOutArea, Capacity, Obstruction, NewProbData);
    %% send a message to all cars to start to stop preventing them to pass the red ligth
        {stop, LightController, LogData} ->  	
	    NewCarsQueque = stop_moving(CarsQueque),
	    io:format("Stop msj received from ~w.~n",[LightController]),
	    write_result(LogData, io_lib:format("Stop msj received from ~w",[LightController])),
	    reply(LightController, updatedStop),
	    lane(LaneId, Type, ConnectedLanes, NewCarsQueque, OutSideArea, Capacity, Obstruction, ProbData);
	{waiting, LightController, _Time, LogData} ->
    %% When recieving a waiting message, update cars times if they can or cannot move
	    NewCarsQueque = waiting(LaneId, CarsQueque, LogData, ConnectedLanes, Obstruction),
	    %%io:format("Waiting msj received from ~w. LogData~p~n",[LightController,LogData]),
	    write_result(LogData, io_lib:format("Waiting msj received from ~w",[LightController])),
	    reply(LightController, updatedWaiting),
	    lane(LaneId, Type, ConnectedLanes, NewCarsQueque, OutSideArea, Capacity, Obstruction, ProbData);
    %% connect_lane msg used to update ConnectedLanes list with any type of lane
	{connect_lane, AdjLane, Pid}  ->     		
    	    %%io:format("{Connected Lanes ~w.~n",[ConnectedLanes]),
    	    NewConnectedLanes = [AdjLane| ConnectedLanes],
     	    %%io:format("{new Connected Lanes ~w.~n",[NewConnectedLanes]),
    	    reply(Pid,ok),
    	    lane(LaneId, Type, NewConnectedLanes, CarsQueque, OutSideArea, Capacity, Obstruction, ProbData);
    %% incoming msg, used when a car arrives to lane
	{incoming, Pid, {car,{Wait, Delay, _Position, Route, PrefLanes}}} ->
	    io:format("Car incoming~n"),
    %% set car to the las position (capacity -1)
	    NewCarData = {car,{Wait,Delay,Capacity - 1, Route, PrefLanes}},
    %% get cars last position to evaluate if a new car can enter the lane
	    {car,{_LastWait, _LastDelay, LastPosition, _LastRout, _PrefLanes}} = get_lastPosition(CarsQueque),
    	    io:format("Car incoming ~w. LastCar Position: ~w~n",[NewCarData, LastPosition]),
    %% in case that lanes capactiy has not been reached and that the last position is free
    %% add the car to the end of the lane, then reply with an ok, if not reply with a full 
    	    LaneLastPos = Capacity - 1,
    	    NoObsBegin = no_obs_on_dispatch(Obstruction, LaneLastPos),
	    case ((length(CarsQueque) < Capacity) and (LastPosition < LaneLastPos) and NoObsBegin)  of
	        true  ->	            
	            NewCarsQueque = lists:reverse([NewCarData|lists:reverse(CarsQueque)]),
	            reply(Pid, ok),
	            lane(LaneId, Type, ConnectedLanes, NewCarsQueque,OutSideArea, Capacity, Obstruction, ProbData);
	        false when NoObsBegin == false ->
	            {siblings, Siblings} = lists:keyfind(siblings, 1, ConnectedLanes),
	            ResObs = check_siblings_obs(Siblings),
	            reply(Pid, ResObs),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData);
	        false ->
	            reply(Pid, full),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData)
	    end;
	{try_transfer,TransferTime, {CallerId, CallerLPid},TransferPosition, CarToTransfer, LogData} ->
	    io:format("OutSchedule of cars incoming. ProbData: ~w ~n", [ProbData]),
	    write_result(LogData, io_lib:format("OutSchedule of cars incoming. ProbData: ~w ~n", [ProbData])),
	    %% Sacar probabilidad de que le den pasada al otro carril
            %% si no le dan pasada, dejarlo en el mismo carril si cambiar
            %% posiciones y aumentar contadores
            RemCap = length(CarsQueque) < Capacity,
            {transfer, List} = lists:keyfind(transfer, 1, ProbData),
            io:format("This: ~w ...Looking for callerPid Caller:~w, List: ~w~n",[LaneId, CallerId, List] ),
            write_result(LogData, io_lib:format("This: ~w ...Looking for callerPid Caller:~w, List: ~w~n",[LaneId, CallerId, List])),
            {_, CedNumCar} = lists:keyfind(CallerId, 1, List),
            io:format("Calling space between cars. RemCap ~w... CedNumCar ~w~n",[RemCap, CedNumCar]),
            write_result(LogData, io_lib:format("Calling space between cars. RemCap ~w... CedNumCar ~w~n",[RemCap, CedNumCar])),
	    case space_between_cars(CarsQueque, TransferPosition, CedNumCar, CarToTransfer, Capacity - 1, TransferTime, RemCap, LogData) of
	        {true, NewCarsQueque, NewCedNumCar} -> 
	            %%NewCarsQueque = transfer_at(Car,ConnectedLanes),	            
	            NewLaneCedList = lists:keyreplace(CallerId,1, List, {CallerId, NewCedNumCar}),
	            NewProbData = lists:keyreplace(transfer,1, ProbData, {transfer, NewLaneCedList}),
	            io:format("Car transfered. Answering to lane ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[NewProbData,NewLaneCedList,NewCedNumCar] ),
	            write_result(LogData, io_lib:format("Car transfered. Answering to lane ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[NewProbData,NewLaneCedList,NewCedNumCar])),
	            io:format("Lane ~w New CarQueque: ~w~n",[LaneId,NewCarsQueque] ),
	            write_result(LogData, io_lib:format("Lane ~w New CarQueque: ~w~n",[LaneId, NewCarsQueque])),
	            reply(CallerLPid, ok),
	            lane(LaneId, Type, ConnectedLanes, NewCarsQueque,OutSideArea, Capacity, Obstruction, NewProbData);
	        {false, _SameQueque, NewCedNumCar} ->
	            NewLaneCedList = lists:keyreplace(CallerId,1, List, {CallerId, NewCedNumCar}),
	            NewProbData = lists:keyreplace(transfer,1, ProbData, {transfer, NewLaneCedList}),
	            io:format("Lane ~w Same CarQueque: ~w~n",[LaneId, CarsQueque]),
	            write_result(LogData, io_lib:format("Lane ~w Same CarQueque: ~w~n",[LaneId, CarsQueque])),
	            reply(CallerLPid, no_space),
	            io:format("Car NOT transfered. Answering to lane ~w ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[{CallerId, CallerLPid}, NewProbData,NewLaneCedList,NewCedNumCar]),	            
	            write_result(LogData, io_lib:format("Car NOT transfered. Answering to lane ~w ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[{CallerId, CallerLPid}, NewProbData,NewLaneCedList,NewCedNumCar])),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction,NewProbData)
	    end;
	   
	{check_for_obs, Pid} ->	    
	    case no_obs_on_dispatch(Obstruction, Capacity - 1)  of
	        true  ->	            	            
	            reply(Pid, ok);        
	        false ->
	            reply(Pid, obs_on_begin)            
	    end,
	    lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData);
	
	{siblingLookup, Pid, AltLaneId, LogData} ->
	    Res = sibling_look_up([main, secondary], ConnectedLanes, AltLaneId),
	    write_result(LogData, io_lib:format("RES after looking up for right sibling on dispatch ~w",[Res])),
	    reply(Pid, Res),
	    lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData);
	    
	{write_down, Pid, Path,LaneIdS} ->
	    write_result(Path, io_lib:format("=======================================",[])),
	    write_result(Path, io_lib:format("START WRITEDOWN CARS INFO FOR LANE: ~w",[LaneIdS])),
	    write_final_data(CarsQueque, Path, LaneId),
	    reply(Pid, finished);
	    	    
	stop -> {ok, normal}
    end.  

%%=======================================================================%%
%%========================WAITING CARS FUNCTIONS=========================%%
%%=======================================================================%%

%% When a waiting message is recieved, cars have to move until they
%% reach the end of line

%%USED FOR WAITING CARS OUTSIDE SOURCES LANES
waiting([], _LogData) ->
    [];
waiting([{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], LogData) -> 
    waiting([{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], [], Position, LogData).
waiting([], UpdatedCars, _LastPosition, LogData) -> 
    NewUpdatedCars =  lists:reverse(UpdatedCars),
    io:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars]),    
    write_result(LogData, io_lib:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars])),
    NewUpdatedCars;
%% update cars that are waiting outsite of sources lanes
waiting([{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], UpdatedCars, _LastPosition, LogData) when Position == -1-> 
    %%io:format("update cars that are waiting outsite of sources lanes~n",[]),    
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position, Route, PrefLanes}} | UpdatedCars], Position, LogData).


%% for normal lanes
waiting(_LaneId, [], _LogData, _ConnectedLanes, _Obstructions) ->
    [];
waiting(LaneId, [{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], LogData, ConnectedLanes, Obstructions) ->
    waiting(LaneId, [{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], [], Position, LogData, ConnectedLanes, Obstructions).
waiting(_LaneId, [], UpdatedCars, _LastPosition, LogData, _ConnectedLanes, []) -> 
    NewUpdatedCars =  lists:reverse(UpdatedCars),
    io:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars]),    
    write_result(LogData, io_lib:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars])),
    NewUpdatedCars;
%% update cars that are waiting outsite of sources lanes
waiting(LaneId, [{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], UpdatedCars, _LastPosition, _LogData, 
  ConnectedLanes, []) when Position == -1-> 
    %%io:format("update cars that are waiting outsite of sources lanes~n",[]),    
    waiting(LaneId, Tail, [{car,{Wait + 1, Delay + 1, Position, Route, PrefLanes}} | UpdatedCars], Position, _LogData, ConnectedLanes, []);  
%% in case that lass position has been reached or if theres a car ahead just update times and do not move the car
waiting(LaneId, [{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], UpdatedCars, LastPosition, _LogData, 
  ConnectedLanes, []) when Position == 0; Position - 1 == LastPosition -> 
    waiting(LaneId, Tail, [{car,{Wait + 1, Delay + 1, Position, Route, PrefLanes}} | UpdatedCars], Position, _LogData, ConnectedLanes, []); 
%% when the next position is greather or equal to 0 and if there is no car update times and move forward
waiting(LaneId, [{car,{Wait,Delay,Position, Route, PrefLanes}}|Tail], UpdatedCars, LastPosition, _LogData, 
  ConnectedLanes, []) when Position - 1 >= 0, Position - 1 /= LastPosition -> 
    waiting(LaneId,Tail, [{car,{Wait + 1, Delay + 1, Position - 1, Route, PrefLanes}} | UpdatedCars], Position - 1, _LogData, ConnectedLanes, []);

waiting(LaneId, CarsQueque, _UpdatedCars, _LastPosition, LogData, ConnectedLanes, [ObsData | _Obstruction])-> 
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
%% try to move the cars to the a sibling lane
    io:format("CALL TRANSFER ON WAIT FUNCTION SHOULD NOT DO DISPATCH ~w, ~n",[List]),    
    write_result(LogData, io_lib:format("CALL TRANSFER ON WAIT FUNCTION SHOULD NOT DO DISPATCH ~w, ~n",[List])),
    UpdatedCarsQueque = transfer_enabled(LaneId, List, length(List), CarsQueque, ObsData, LogData),
    %%NewProbData = lists:keyreplace(transfer,1, ProbData, {transfer, NewCedCarNum}),
    UpdatedCarsQueque.
    
%%=================================================================================================================================
%%=================================================================================================================================

%%=======================================================================%%
%%=========================MOVING CARS FUNCTIONS=========================%%
%%=======================================================================%%

%% When a move message is recieved
move_cars([], _ConnectedLanes, _Obstruction, UpdatedCars, _LanCap, ProbData, _LaneId, NewOutArea, LogData) -> 
    io:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars]),
    write_result(LogData, io_lib:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars])),
    {lists:reverse(UpdatedCars), ProbData, NewOutArea};
%% if car is still on lane move it forward
move_cars([{car,{Wait,Delay, Position, Route, PrefLanes}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData) when Position - 1 >= 0 -> 
    move_cars(Tail, ConnectedLanes,[], [{car,{Wait + 1,Delay, Position - 1, Route, PrefLanes}} | UpdatedCars], LanCap, ProbData, LaneId, NewOutArea, LogData);
%% if car has reached the end of line, dispatch (send) car to one of connected lanes
move_cars([{car,{Wait,Delay, Position, Route, PrefLanes}}|Tail], ConnectedLanes, Obs, UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData) when Position - 1 < 0 -> 
    %%Dispatch Cars
    io:format("Prob dispatch: ~w.~n",[ProbData]),
    write_result(LogData, io_lib:format("Prob dispatch: ~w.~n",[ProbData])),
    {dispatch, TurnCarNum} = lists:keyfind(dispatch, 1, ProbData),
    %%Get parent lane 
    S = atom_to_list(LaneId),
    ParentLaneId = list_to_atom(string:concat(string:substr(S,8,2), string:substr(S,5,3))),
    
    %% try to dispatch car to connected lane and get result of it
    {Res, NewTurnCarNum} = prepare_car_dispatch({car,{Wait,Delay, Position, Route, PrefLanes}}, ConnectedLanes, Tail,TurnCarNum, {LaneId, ParentLaneId},LogData),
    NewProbData = lists:keyreplace(dispatch,1, ProbData, {dispatch, NewTurnCarNum}),
    io:format("Dispatch result: ~w.~n",[Res]),
    write_result(LogData, io_lib:format("Dispatch result: ~w.~n",[Res])),
    %% if car was able to move to the next lane, continue with remaining cars, if not stop moving the rest of
    %% the cars
    case Res of
        {reply, transfered, Car}   -> move_cars(Tail, ConnectedLanes, Obs, UpdatedCars, LanCap,NewProbData, LaneId, [Car|NewOutArea], LogData);
        {reply, transfered}        -> move_cars(Tail, ConnectedLanes, Obs, UpdatedCars, LanCap,NewProbData, LaneId,NewOutArea, LogData);
        {reply, error, NewUpdated} -> io:format("CALL STOP MOVING ~w  position ~w.~n",[NewUpdated, Position]),
        			      write_result(LogData, io_lib:format("CALL STOP MOVING ~w  position ~w.~n",[NewUpdated, Position])),
           			      {stop_moving(NewUpdated, Position), NewProbData, NewOutArea}
    end;
%% in case that thers and obstruction on the lane
move_cars(CarsQueque, ConnectedLanes, [ObsData | _Obstruction], _UpdatedCars, 
            _LanCap, ProbData, LaneId, NewOutArea, LogData)-> 
%% get probability for the current car to see if its enable to 
%% cross to a sibling lane
%% if it was not able, leave it on the same lane and update times
%% and cotinue with the rest of the list
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
    %%{transfer, CedCarNum} = lists:keyfind(transfer, 1, ProbData),
%% try to move the cars to the a sibling lane
    UpdatedCarsQueque = transfer_enabled(LaneId, List, length(List), CarsQueque, ObsData, LogData),
    %%NewProbData = lists:keyreplace(transfer,1, ProbData, {transfer, NewCedCarNum}),
    {UpdatedCarsQueque,ProbData,NewOutArea} .
    
%%=======================================================================%%
%%=======================================================================%%
%%=======================================================================%%  

%%=======================================================================%%
%%====================DISPATCH CARS FUNCTIONS============================%%
%%=======================================================================%%

%% INPUT: obstructions list of the lane,
%%	  lane last position
%% OUTPUT: true is there is not obstruction on that position
%%	   false is there is one.
no_obs_on_dispatch([], _LaneLastPos) ->
    true;
no_obs_on_dispatch([{_Obs, _Begin, LaneLastPos} | _Tail], LaneLastPos) ->
    false;
no_obs_on_dispatch([_Obs | Tail], LaneLastPos) ->
    no_obs_on_dispatch(Tail, LaneLastPos).

%% INPUT: Lanes list of to verify for obs on the begining,
%% OUTPUT: {obs_on_begin, ALTERNATIVE LANE TO TAKE}
check_siblings_obs([]) ->
    {obs_on_begin, none};
check_siblings_obs([{LaneId, LanePid} | Tail]) ->
    LanePid ! {check_for_obs, self()},
    receive
        {reply, ok}           -> {obs_on_begin, {LaneId, LanePid}};
        {reply, obs_on_begin} -> check_siblings_obs(Tail) 
    end.

    
%% Entradas: Begin (posicion inicial de la obstruccion)
%%	     End (Posicion final de la obstruccion)
%%	     CarPos(Posicion que va a ocupar el carro
%% Salidas:  {true, obs_in_way} cuando el carro caería en la obstruccion
%%	     {false, obs_away} cuando el carro puede pasar si problemas
obstacule_located(Begin, End, CarPos) when CarPos >= Begin, CarPos =< End ->
    {true, obs_in_way};
obstacule_located(_Begin, _End, _CarPos) ->
    {false, obs_away}.

%%Get the probability that the car goes either straight or turn in the corner
%%This calls car_dispatch and according to the prob gets the right lane to send the car
prepare_car_dispatch(Car, ConnectedLanes, CarsQueque,TurnCarNum, {LaneId, ParentLaneId},LogData) when TurnCarNum == -1 ->
    %{Type, List} = lists:keyfind(main, 1, ConnectedLanes),
    %%TargetLane = get_enabled_lane([main, secondary], ConnectedLanes, [{LaneId, self()}]),
    TargetLane = get_target_lane([main, secondary], ConnectedLanes, [{LaneId, self()}], Car),
    {siblings, SList} = lists:keyfind(siblings, 1, ConnectedLanes),
    {{Type, List}, NewCarData} = TargetLane,
    io:format("Prepare dispatch to straigth lane on NO turnCarNum: ~w.~n",[{Type, List}]),
    write_result(LogData, io_lib:format("Prepare dispatch to straigth lane on NO turnCarNum: ~w.~n",[{Type, List}])),
    {Res, _OldTurnCar} = car_dispatch(NewCarData, List, CarsQueque, {LaneId,self(), ParentLaneId},SList, LogData, TurnCarNum),
    {Res, TurnCarNum};
prepare_car_dispatch(Car, ConnectedLanes, CarsQueque,TurnCarNum, {LaneId, ParentLaneId},LogData) when TurnCarNum > 0 ->
    %%{Type, List} = lists:keyfind(main, 1, ConnectedLanes),
    %%TargetLane = get_enabled_lane([main, secondary], ConnectedLanes, [{LaneId, self()}]),
    TargetLane = get_target_lane([main, secondary], ConnectedLanes, [{LaneId, self()}], Car),
    {siblings, SList} = lists:keyfind(siblings, 1, ConnectedLanes),
    {{Type, List}, NewCarData} = TargetLane, 
    io:format("Prepare dispatch to straigth lane: ~w. TurnCarNum ~w ~n",[{Type, List}, TurnCarNum]),
    write_result(LogData, io_lib:format("Prepare dispatch to straigth lane: ~w. TurnCarNum ~w ~n",[{Type, List}, TurnCarNum])),
    Data = car_dispatch(NewCarData, List, CarsQueque, {LaneId,self(), ParentLaneId}, SList, LogData, TurnCarNum),
    io:format("DATA~w ~n",[Data]),
    write_result(LogData, io_lib:format("DATA~w ~n",[Data])),
    {Res, NewTurnCarNum} = Data,
    io:format("return after prepare dispatch to straigth lane: ~w . TurnCarNum  ~w ~n",[Res, NewTurnCarNum]),
    write_result(LogData, io_lib:format("return after prepare dispatch to straigth lane: ~w .  ~w ~n",[Res, NewTurnCarNum])),
    %%{Res, TurnCarNum - 1};
    {Res, NewTurnCarNum};
prepare_car_dispatch(Car, ConnectedLanes, CarsQueque, TurnCarNum, {LaneId, ParentLaneId},LogData) when TurnCarNum == 0 ->
    %%{Type, List} = lists:keyfind(secondary, 1, ConnectedLanes),
    NewTurnCarNum = new_turn(),    
    %%TargetLane = get_enabled_lane([secondary,main], ConnectedLanes, [{LaneId, self()}]),
    TargetLane = get_target_lane([secondary,main], ConnectedLanes, [{LaneId, self()}], Car),
    {siblings, SList} = lists:keyfind(siblings, 1, ConnectedLanes),
    {{Type, List}, NewCarData} = TargetLane,
    io:format("Prepare dispatch to alt lane: ~w.~n",[{Type, List}]),
    write_result(LogData, io_lib:format("Prepare dispatch to alt lane: ~w.~n",[{Type, List}])),
    Data = car_dispatch(NewCarData, List, CarsQueque, {LaneId,self(), ParentLaneId}, SList, LogData, TurnCarNum),
    io:format("DATA~w ~n",[Data]),
    write_result(LogData, io_lib:format("DATA~w ~n",[Data])),
    {Res, _OldTurnCar} = Data,
    io:format("return after prepare dispatch to alt lane: ~w NewTurnCar ~w ~n",[Res, NewTurnCarNum]),
    write_result(LogData, io_lib:format("return after prepare dispatch to alt lane: ~w NewTurnCar ~w ~n",[Res, NewTurnCarNum])),
    %%{car_dispatch(Car, List, CarsQueque, self(),LaneId), NewTurnCarNum}.
    {Res, NewTurnCarNum}.

%%%
%% INPUT: LanesOrder: LIst of lanes to look for (main, secondary)
%%	 ConnectedLanes: a list of tuples of all lanes (main, secondary, siblings) connected to the current
%%	 AuxLane: Default lane data in case of no matches.
%%	 Car: Data of the car to be evaluated
%% OUTPUT: Target Lane to be used for dispatch
%%
%% DESCRIPTION:
%% get the target lane according to car prefered lane (first), Main lane (second), secondary(third) 
%% or diferent lane in case that there are no available space because of obstructions
%% when there is no prefered target lane, get the enabled lane acording to LanesOrder (Options: main, secondary)
get_target_lane(LanesOrder, ConnectedLanes, AuxLane, {car,{Wait,Delay, Position, Route, []}}) ->
    {get_enabled_lane(LanesOrder, ConnectedLanes, AuxLane, Route), {car,{Wait,Delay, Position, Route, []}}};

% in case that the car has a prefered lane use that instead of previous selected (priority: TOP)  
get_target_lane(LanesOrder, ConnectedLanes, AuxLane, {car,{Wait,Delay, Position, Route, [TLane | Tail]}}) ->
    case check_lanes_route(Route, [TLane]) of
        true  -> {{prefered, [TLane]}, {car,{Wait,Delay, Position, Route, Tail}}};
        false -> {get_enabled_lane(LanesOrder, ConnectedLanes, AuxLane, Route), {car,{Wait,Delay, Position, Route, Tail}}}
    end.
    
  
%%Input: LIst of lanes to look for (main, secondary), 
%%	 ConnectedLanes: a list of tuples of all lanes (main, secondary, siblings) connected to the current
%%	 Auxlanes: Default lane data in case of no matches.
%%Output: Target Lane to be used for dispatch that is enabled
get_enabled_lane([], _ConnectedLanes, AuxLane, _Route) ->
    {outside, AuxLane};  
get_enabled_lane([TargetItem | TargetOrder], ConnectedLanes, AuxLane, Route) ->
    {Type, List} = lists:keyfind(TargetItem, 1, ConnectedLanes),
    case List of
        [{}] -> get_enabled_lane(TargetOrder, ConnectedLanes, AuxLane, Route);
        []   -> get_enabled_lane(TargetOrder, ConnectedLanes, AuxLane, Route);
        _Ok  -> 
        	case check_lanes_route(Route, List) of
        	    true  -> {Type, List};
        	    false -> get_enabled_lane(TargetOrder, ConnectedLanes, AuxLane, Route)
        	end
    end.


%%%
%% INPUT: Route: LIst of streets or avenues that the car has taken so far
%%	 LaneList: List of tuples lanes ({LaneId, LanePid}) that could be the current target for the car to dispatch
%% OUTPUT: validated Target Lane to be used for dispatch to aviod pass twice or more on the same lane
%%
%% DESCRIPTION:
%% get the target lane list and check it against the lanes on route list if theres a match skip that lane and continue
%% with the next one on the list, when end of list is reached return false

check_lanes_route([], _LaneList) ->
    true;
check_lanes_route(Route,  [{LaneId, _LanePid} | _Tail]) ->
    S = atom_to_list(LaneId),
    ParentLaneId = list_to_atom(string:concat(string:substr(S,8,2), string:substr(S,5,3))),
    check_lanes_route_aux(Route, ParentLaneId).
    
check_lanes_route_aux([], _PTLane) ->
    true; 
check_lanes_route_aux([PLane | _RouteTail], PLane) ->
    false; %% the car has already passed trought this lane
check_lanes_route_aux([_PLane | RouteTail], PLane) ->
    check_lanes_route_aux(RouteTail, PLane).

%% GET the car and try to
%% Send it to connected lane
car_dispatch(Car, [], CarsQueque, _CLaneData ,_SList,LogData, _TurnCarNum) ->
    io:format("Nolane. Adding to wait list~n",[]),
    write_result(LogData, io_lib:format("Nolane. Adding to wait list~n",[])),
    {reply, error, [Car|CarsQueque]};
%% if the source lane and the target is the same the car has to leave the area.
car_dispatch({car,{Wait,Delay, Position, Route, PrefLanes}}, [{_LaneId, LanePid} | _Tail], _CarsQueque, {_CLaneId,CLanePid, CPLaneId},
  _SList,LogData, TurnCarNum) when LanePid == CLanePid ->
    io:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, {car,{Wait,Delay, Position, [CPLaneId|Route], PrefLanes}}]),
    write_result(LogData, io_lib:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, {car,{Wait,Delay, Position, [CPLaneId|Route], PrefLanes}}])),
    {{reply, transfered, {car,{Wait,Delay, Position, [CPLaneId|Route], PrefLanes}}}, TurnCarNum - 1};
%% if its a differente lane, send an incoming msg to let it now a new car is commingNewTurnCarNum = new_turn(),    
car_dispatch(Car = {car,{Wait,Delay, Position, Route, PrefLanes}}, [{LaneId, LanePid} | _Tail], CarsQueque,{CLaneId,CLanePid, CPLaneId},
  SList,LogData, TurnCarNum) when LanePid /= CLanePid ->
    PreSendCar = {car,{Wait,Delay, Position, [CPLaneId | Route], PrefLanes}},
    io:format("Lane ~w to dispatch ~w. Car ~w ~n",[CPLaneId, LaneId, Car]),
    write_result(LogData, io_lib:format("Lane ~w to dispatch ~w. Car ~w ~n",[CPLaneId, LaneId, Car])),
    LanePid ! {incoming, self(), PreSendCar},
    receive
        %% if a "full" reply is received then return original CarsQueque with the car
        {reply, full}  ->
            io:format("Lane ~w its full on car dispatch. Adding to wait list~n",[LaneId]),
            write_result(LogData, io_lib:format("Lane ~w its full on car dispatch. Adding to wait list~n",[LaneId])),
	    {{reply, error, [{car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}}|CarsQueque]}, TurnCarNum};
            %%CarsQuequeUpdated = waiting([Car|CarsQueque], []);
        %% if a "ok" reply is received then return transfered reply
        {reply, {obs_on_begin, {AltLaneId, AltLanePid}}} ->
            io:format("Lane ~w has an obstruction on the begin of the street. Try to sibling ~w~n",[LaneId,AltLaneId]),
            write_result(LogData, io_lib:format("Lane ~w has an obstruction on the begin of the street. Try to sibling ~w~n",[LaneId,AltLaneId])),
            AltSibLane = get_right_sibling(SList, {AltLaneId, AltLanePid}, LogData),
            case AltSibLane of
                _Others when TurnCarNum == 0 -> %% if its a car turning try to dispatch to other sibling, do not do transfer
                    io:format("The car needed to turn, leaving on the same lane and call dispatch to target's sibling~n",[]),
                    write_result(LogData, io_lib:format("The car needed to turn, leaving on the same lane and call dispatch to target's sibling~n",[])),
                    car_dispatch(Car, [{AltLaneId, AltLanePid}], CarsQueque, {CLaneId,CLanePid, CPLaneId}, SList, LogData, TurnCarNum);
                {error, no_macth} -> 
                    io:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[]),
                    write_result(LogData, io_lib:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[])),
                    {{reply, error, [{car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}}|CarsQueque]}, TurnCarNum};                          
                _Others ->%%only do transfer if is to go straight not to turn
                    CarWPrefered = {car,{Wait,Delay, Position, Route, [AltSibLane | PrefLanes]}},
                    try_alt_lane(CLaneId, AltSibLane, CarWPrefered, LogData, CarsQueque, TurnCarNum)
            end;        
        {reply, {obs_on_begin, none}} ->
            io:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[]),
            write_result(LogData, io_lib:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[]));
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            write_result(LogData, io_lib:format("Card Added to lane ~w~n",[LaneId])),
            {{reply, transfered}, TurnCarNum - 1}
            %%CarsQuequeUpdated = waiting(CarsQueque,[]), 
            
    end.


%%TRY to send the car to the sibling lane to avoid obs on next street or avenue
%% to get the right sibling, use the SList (siblings list) to determine which one 
%% has the connection to the altLaneId
try_alt_lane(CLaneId, {AltLaneId, AltLanePid}, Car, LogData,CarsQueque, TurnCarNum) ->
    Res = attemp_transfer(CLaneId, {AltLaneId, AltLanePid}, Car, LogData),  
    case Res of 
        {ok, nothing} -> {{reply, transfered}, TurnCarNum - 1};
        {no_space, NewCarData} -> {{reply, error, [NewCarData|CarsQueque]}, TurnCarNum}
    end.
    
    
%% gets the right sibling to transfer the dispatch car so it can try a dispatch 
%% to prefered lane
get_right_sibling([], _AltLaneData, _LogData) ->
    {error, no_macth};
get_right_sibling([{SiblingId, SiblingPid} | SList], {AltLaneId, AltLanePid}, LogData) ->
    SiblingPid ! {siblingLookup, self(), AltLaneId, LogData},
    receive
       {reply, true}  -> io:format("Right Sibling found ~w~n",[{SiblingId, SiblingPid}]),
       			 write_result(LogData, io_lib:format("Right Sibling found ~w~n",[{SiblingId, SiblingPid}])),
                         {SiblingId, SiblingPid};
       {reply, false} -> io:format("This is not the right sibling, continue lookup~n",[]),
       			 write_result(LogData, io_lib:format("This is not the right sibling, continue lookup",[])),
       		         get_right_sibling(SList, {AltLaneId, AltLanePid}, LogData)
    end.
    
sibling_look_up([], _ConnectedLanes, _TLaneId) -> 
    false;
sibling_look_up([TargetItem | TargetOrder], ConnectedLanes, TLaneId) ->
    {_Type, List} = lists:keyfind(TargetItem, 1, ConnectedLanes),
    case match_lane(List, TLaneId) of
        false -> sibling_look_up(TargetOrder, ConnectedLanes, TLaneId);
        true -> true
    end.

match_lane([], _TLaneId) ->
    false;
match_lane([{TLaneId, _LanePid} | _Tail], TLaneId) ->
    true;
match_lane([_ILane | Tail], TLaneId) ->
    match_lane(Tail, TLaneId).
    
%%=======================================================================%%
%%=======================================================================%%
%%=======================================================================%%    


%%=======================================================================%%
%%====================TRANSFER CARS FUNCTIONS============================%%
%%===========USED WHEN THERE ARE OBSTRUCTIONS ON LANES===================%%
%%=======================================================================%%    

%% In case that there are obstructions on the road, cars will try to change lanes (siblings)
%% and after that we get an updated list of remaining cars and move the others forward.
%% a lane has a max of 2 siblings so we use SiblingsNum var to determine which one to get
transfer_enabled(CurrentLaneId, List, SiblingsNum, CarsQueque, ObsData, LogData) when SiblingsNum == 1 ->
    [Sibling | _Tail] = List,
    io:format("Attemp with just one sibling ~w~n",[{Sibling, CarsQueque, [], ObsData}]),
    write_result(LogData, io_lib:format("Attemp with just one sibling ~w~n",[{Sibling, CarsQueque, [], ObsData}])),
    %%attemp_transfer(Sibling, CarsQueque, [], End, CedCarNum);
    attemp_transfer(CurrentLaneId, Sibling, CarsQueque, [], ObsData, LogData, -1);
transfer_enabled(CurrentLaneId, List, SiblingsNum, CarsQueque, ObsData, LogData) when SiblingsNum > 1 ->
    TransferSibling = random:uniform(SiblingsNum),    
    Sibling = lists:nth(TransferSibling, List),
    io:format("Attemp with two sibling~n",[]),
    write_result(LogData, io_lib:format("Attemp with two sibling~n",[])),
    attemp_transfer(CurrentLaneId, Sibling, CarsQueque, [], ObsData, LogData, -1);
transfer_enabled(_CurrentLaneId, List, SiblingsNum, CarsQueque, ObsData, LogData) ->
    io:format("You're soo fucked up ~w~n",[{List,SiblingsNum, CarsQueque, [], ObsData}]),
    write_result(LogData, io_lib:format("You're soo fucked up ~w~n",[{List,SiblingsNum, CarsQueque, [], ObsData}])),
    CarsQueque.


%% TODO:HACER que el carro trate de pasarse si puede cambia de linea si no, avanza siempre y cuando no haya obstrucion u otro carro
%% Attemp to transfer each car on the lane to sibling line    
%% REMOVED: CedNumCar, because sibling lane has the CedNumtransfer for this lane
attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, {car,{Wait,Delay, Position, Route, PrefLanes}}, LogData) ->
    %% when there is an obstacule at the begining of the next lane selected for dispatch
    io:format("CALLING SIBLING FOR TRANSFER on ~w to ~w ~n after dispatch error",[CurrentLaneId, {SiblingId, SiblingPid}]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER on ~w to ~w ~n after dispatch error",[CurrentLaneId, {SiblingId, SiblingPid}])),    
    SiblingPid ! {try_transfer,before, {CurrentLaneId, self()},Position, {car,{Wait,Delay, Position, Route, PrefLanes}}, LogData},
    receive
       {reply, ok}    -> io:format("transfer succeded for dispatch ~w~n",[{SiblingId, SiblingPid}]),
       			 write_result(LogData, io_lib:format("transfer succeded for dispatch ~w~n",[{SiblingId, SiblingPid}])),
                         {ok, nothing};
       {reply, no_space} -> io:format("transfer failed for dispatch~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed for dispatch~n",[])),
       		         {no_space, {car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}}}
    end.



attemp_transfer(_CurrentLaneId, _Sibling, [], UpdatedCars, _ObsData, _LogData, _LastPosition) ->
    lists:reverse(UpdatedCars);
%% if the car has reached the obstruction try to transfer, if "ok" move the rest of cars, if not test with other cars
attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail], 
  UpdatedCars, {Obs,ObsBeginPosition, ObsEndPosition}, LogData, _LastPosition) when Position -1 == ObsEndPosition->
    %% When they reached the obstacule stop the cars on the lane and just update times
    io:format("CALLING SIBLING FOR TRANSFER when position reached on ~w to ~w ~n",[CurrentLaneId, {SiblingId, SiblingPid}]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER when position reached on ~w to ~w ~n",[CurrentLaneId, {SiblingId, SiblingPid}])),
    %%SiblingPid ! {try_transfer,atpoint, self(), CedCarNum, {car,{Wait,Delay, Position, Route, PrefLanes}}},
    SiblingPid ! {try_transfer,atPoint, {CurrentLaneId, self()}, Position, {car,{Wait,Delay, Position, Route, PrefLanes}}, LogData},
    receive
        {reply, ok}    -> io:format("transfer succeded with two sibling~n",[]),
        		  write_result(LogData, io_lib:format("transfer succeded with two sibling~n",[])),        	          
        	          %%keep_moving(Tail, ObsEndPosition,LogData);
        	          attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData, -1);
        {reply, no_space} -> 
        		  io:format("transfer failed with two sibling~n",[]),
        		  write_result(LogData, io_lib:format("transfer failed with two sibling~n",[])),
        	          %%stop_moving(Tail, Position, [ {car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}} | UpdatedCars])
        	          attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, [{car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,Position)
    end;

%%attemp_transfer(Sibling, [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail], UpdatedCars, ObsPosition, 
%%  CedCarNum) when Position -1 /= ObsPosition ->
%%    %% When they have not reached the obstacule 
%%   attemp_transfer(Sibling, Tail, [{car,{Wait + 1,Delay, Position - 1, Route, PrefLanes}} | UpdatedCars], ObsPosition, CedCarNum).

%%TODO: THIS SHOULD BE USED FOR TRANSFER AT ANY PLACE OF THE LANE
attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail], 
  UpdatedCars, ObsData = {Obs, ObsBeginPosition, ObsEndPosition}, LogData, LastPosition) when Position - 1 > ObsEndPosition ->
    %% when they haven't reached the obstacule try to pass the car to the sibling lane if possible pass if not continue moving
    %% until it reaches the obstacle
    io:format("CALLING SIBLING FOR TRANSFER when position NOT reached on ~w to ~w ~n",[CurrentLaneId, {SiblingId, SiblingPid}]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER when position NOT reached on ~w to ~w ~n",[CurrentLaneId, {SiblingId, SiblingPid}])),
    %%SiblingPid ! {try_transfer, before, self(), {car,{Wait,Delay, Position, Route, PrefLanes}}},
    SiblingPid ! {try_transfer,before, {CurrentLaneId, self()},Position, {car,{Wait,Delay, Position, Route, PrefLanes}}, LogData},
    receive
       {reply, ok}    -> io:format("transfer succeded with two sibling ~w~n",[{{SiblingId, SiblingPid}, Tail, UpdatedCars, ObsData}]),
       			 write_result(LogData, io_lib:format("transfer succeded with two sibling ~w~n",[{{SiblingId, SiblingPid}, Tail, UpdatedCars, ObsData}])),
                         attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData, -1);
       {reply, no_space} when Position -1 >= 0, Position - 1 > LastPosition -> io:format("transfer failed with two sibling~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed with two sibling~n",[])),
       			 NewPosition = Position - 1,
       		         attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, [{car,{Wait + 1,Delay, NewPosition, Route, PrefLanes}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,NewPosition);       
       {reply, no_space} -> io:format("transfer failed with two sibling~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed with two sibling",[])),
       		         attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, [{car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,Position)
    end;
    
attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail], 
  UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData,LastPosition) when Position - 1 < ObsBeginPosition, Position - 1 >= 0, Position - 1 > LastPosition ->
    %% when they aren't in the way of the obstacule do not try to pass the car to the sibling lane 
    io:format("MOVING NORMAL, NOT CALLING SIBLING FOR TRANSFER On ~w ~n",[CurrentLaneId]),
    write_result(LogData, io_lib:format("MOVING NORMAL, NOT CALLING SIBLING FOR TRANSFER On ~w ~n",[CurrentLaneId])),    
    NewPosition = Position - 1,
    attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, [{car,{Wait + 1,Delay, NewPosition, Route, PrefLanes}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,NewPosition);
    
attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail], 
  UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData,LastPosition) when Position - 1 < ObsBeginPosition, Position - 1 < 0; Position - 1 == LastPosition ->
    %% when they aren't in the way of the obstacule do not try to pass the car to the sibling lane 
    io:format("CANT MOVE NORMAL, NOT CALLING SIBLING FOR TRANSFER, NEXT POSITION: ~w IS OCCUPIEDED On ~w ~n",[{Position - 1, LastPosition}, CurrentLaneId]),
    write_result(LogData, io_lib:format("CANT MOVE NORMAL, NOT CALLING SIBLING FOR TRANSFER, NEXT POSITION: ~w IS OCCUPIEDED On ~w ~n",[{Position - 1, LastPosition}, CurrentLaneId])),    
    attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, Tail, [{car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,Position);

attemp_transfer(CurrentLaneId, {_SiblingId, _SiblingPid}, [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail], 
  _UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData, _LastPosition) when Position >= ObsBeginPosition, Position  =< ObsEndPosition ->
    %% when they aren't in the way of the obstacule do not try to pass the car to the sibling lane 
    io:format("LOCATION ERROR CAR POSITION : ~w IS ON OBSTRUCTION LOCATION: ~w IS OCCUPIEDED On ~w ~n",[Position, {Obs, ObsBeginPosition, ObsEndPosition}, CurrentLaneId]),
    write_result(LogData, io_lib:format("LOCATION ERROR CAR POSITION : ~w IS ON OBSTRUCTION LOCATION: ~w IS OCCUPIEDED On ~w ~n",[Position, {Obs, ObsBeginPosition, ObsEndPosition}, CurrentLaneId])),    
    [{car,{Wait,Delay, Position, Route, PrefLanes}} | Tail].

%%check space between cars CarsQueque, Position,TransferTime,ProbData
space_between_cars([], _TransferPosition,ProbData, CarToTransfer,_Capacity, _TransferTime, _RemCap, LogData) ->
    io:format("space between cars... No cars on lane ~n"),
    write_result(LogData, io_lib:format("space between cars... No cars on lane ~n",[])),
    {true, [CarToTransfer|[]], ProbData};
space_between_cars(CarsQueque, _TransferPosition,ProbData, _CarToTransfer,_Capacity, _TransferTime, false, LogData) ->
    io:format("NO MORE SPACE ON LANE~n"),
    write_result(LogData, io_lib:format("NO MORE SPACE ON LANE~n",[])),
    {false, CarsQueque, ProbData};
space_between_cars(CarsList, TransferPosition,ProbData, CarToTransfer, Capacity, TransferTime, true, LogData) ->
   [Car | Tail] = CarsList,
   io:format("space between cars... More than 1 car on lane~n"),
   write_result(LogData, io_lib:format("space between cars... More than 1 car on lane~n",[])),
   is_space_enabled(Car, Tail, TransferPosition,ProbData, CarToTransfer, [],Capacity, TransferTime, LogData).
%%space_between_cars(CarsList, TransferPosition,ProbData, CaToTransfer,Capacity, _TransferTime) when length(CarsList) == 1 ->
%%   [Car | _Tail] = CarsList,
%%   io:format("space between cars... just 1 car on lane~n"),
%%   is_space_enabled(Car, TransferPosition, ProbData, CaToTransfer, CarsList, Capacity).


is_space_enabled({car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}}, [], TransferPosition,ProbData, 
  CaToTransfer, UpdatedCars, Capacity, _TransferTime, LogData) when PositionL < TransferPosition, TransferPosition =< Capacity ->
    %%CUANDO EL UNICO CARRO EN LA LINEA o el ultimo carro SE ENCUENTRA ANTES DE LA POSICION A PASAR
    io:format("Transfer car after last or only remaining car on lane~n"),
    write_result(LogData, io_lib:format("Transfer car after last or only remaining car on lane~n",[])),
    NewUpdated = [{car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}} | UpdatedCars],
    AddCarList = [CaToTransfer | NewUpdated],
    io:format("Transfer car after last. CarList and probdata ~w~n",[{AddCarList, ProbData}]),
    write_result(LogData, io_lib:format("Transfer car after last. CarList and probdata ~w~n",[{AddCarList, ProbData}])),
    {true, lists:reverse(AddCarList), ProbData};

is_space_enabled({car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}}, CarsQueque, TransferPosition,ProbData, 
  CaToTransfer, UpdatedCars, _Capacity, TransferTime, LogData) when PositionL > TransferPosition ->
    %%CUANDO EL PRIMER CARRO EN LA LINEA SE ENCUENTRA DESPUÉS DE LA POSICION A PASAR
    io:format("Transfer car before first car or only remaining car on lane~n"),
    write_result(LogData, io_lib:format("Transfer car before first car or only remaining car on lane~n",[])),
    LocationRes = in_range(PositionL, TransferPosition),
    io:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes]),
    write_result(LogData, io_lib:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes])),
    evaluate_location_result(LocationRes,{car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}},UpdatedCars, CaToTransfer,
        CarsQueque, ProbData,  TransferTime);

is_space_enabled({car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}}, [], TransferPosition,ProbData, 
  CaToTransfer, UpdatedCars, _Capacity, TransferTime, LogData) ->
    io:format("Transfer car is on the same position of the other car on lane~n"),
    write_result(LogData, io_lib:format("Transfer car is on the same position of the other car on lane~n",[])),
    LocationRes = in_range(PositionL, TransferPosition),
    io:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes]),
    write_result(LogData, io_lib:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes])),
    evaluate_location_result(LocationRes,{car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}},UpdatedCars, CaToTransfer,
        [], ProbData,  TransferTime);

is_space_enabled({car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}}, CarsQueque = [{car,{Wait,Delay, Position, Route, PrefLanes}} | CarsTail], TransferPosition,ProbData, 
  CaToTransfer, UpdatedCars, Capacity, TransferTime, LogData) ->
    %%CUANDO EL PRIMER CARRO EN LA LINEA SE ENCUENTRA DESPUÉS DE LA POSICION A PASAR
    io:format("Transfer car between two cars or to the end~n"),
    write_result(LogData, io_lib:format("Transfer car between two cars or to the end~n",[])),
    LocationRes = in_range(PositionL,Position,  TransferPosition),
    io:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, Position, TransferPosition}, LocationRes]),
    write_result(LogData, io_lib:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, Position, TransferPosition}, LocationRes])),
    case LocationRes of
        {false, no_in_range} ->
           is_space_enabled({car,{Wait,Delay, Position, Route, PrefLanes}}, CarsTail, 
              TransferPosition, ProbData, CaToTransfer, [{car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}}|UpdatedCars], Capacity, TransferTime, LogData);    
        Other -> 
           evaluate_location_result(Other,{car,{WaitL, DelayL, PositionL, RouteL, PrefLanesL}},UpdatedCars, CaToTransfer,
              CarsQueque, ProbData,  TransferTime)
    end.
    
evaluate_location_result ({true, Diff}, FirstCar, UpdatedCars, 
  CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff >= 2  ->
    NewUpdated = [FirstCar | UpdatedCars],
    AddCarList = [CaToTransfer | NewUpdated],
    {true, lists:append(lists:reverse(AddCarList), CarsQueque), ProbData};

evaluate_location_result ({true, Diff}, FirstCar, UpdatedCars, 
  CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff < 2, ProbData == 0  ->
    NewUpdated = [FirstCar | UpdatedCars],
    AddCarList = [CaToTransfer | NewUpdated],
    NewAllowPass = new_transfer(),
    {true, lists:append(lists:reverse(AddCarList), CarsQueque), NewAllowPass};

evaluate_location_result ({true, Diff}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when Diff < 2, ProbData > 0, TransferTime == atPoint ->          
    NewUpdated = [FirstCar | UpdatedCars],
    {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData - 1};
    
evaluate_location_result ({true, Diff}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when Diff < 2, ProbData > 0, TransferTime == before ->          
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData};

evaluate_location_result ({true, Diff}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff < 2, ProbData < 0 ->          
     io:format("ERROR ON PROBDATA"),
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData};
    
evaluate_location_result ({false, no_space}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when TransferTime == atPoint, ProbData > 0 -> %% don't move cars to next position on same lane
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData - 1};

evaluate_location_result ({false, no_space}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when TransferTime == atPoint, ProbData == 0 -> %% don't move cars to next position on same lane
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData};

evaluate_location_result ({false, no_space}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when TransferTime == before -> %% move cars to next position on same lane
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData}.

%%TODO: REVISAR PARA EL CASO DE EL ULTIMO CARRO EN LA FILA Y EL CARRO POR AGREGAR    
%% Check if TransferPosition(Match) is located between the current position of first car (Min)
%% and the next one (Max).
in_range(Min, Max, Match) when Match > Min, Match < Max->
    {true, Max - Match};
in_range(Min, Max, Match) when Match > Min, Match > Max->
    {false, no_in_range};
in_range(Min, Max, Match) when Match == Min; Match == Max->
    {false, no_space};
in_range(_Min, _Max, _Match) -> 
    {false, -1}. 
in_range(Min, Match) when Match < Min->
    {true, Min - Match};
in_range(Min, Match) when Match == Min->
    {false, no_space};
in_range(_Min,_Match) -> 
    {false, -1}.

%%Update probData
allow_pass_update(ProbData, Position) ->
    {transfer, List} = lists:keyfind(transfer, 1, ProbData),
    NewTransferList = allow_pass_update(List,Position, []),
    lists:keyreplace(transfer,1, ProbData, {transfer, NewTransferList}).
allow_pass_update(-1, _Position, _NewTransferList) ->
    -1;
allow_pass_update([], _Position, NewTransferList) ->
    NewTransferList;
allow_pass_update([{LaneId, APass, Position} | Tail], Position, NewTransferList) ->
    allow_pass_update(Tail, Position, [{LaneId, APass - 1, Position} | NewTransferList]);
allow_pass_update([Transfer | Tail], Position, NewTransferList) ->
    allow_pass_update(Tail, Position, [Transfer| NewTransferList]).

%%=======================================================================%%
%%=======================================================================%%
%%=======================================================================%%    


%%=======================================================================%%
%%====================GENERAL FUNCTIONS==================================%%
%%=======================================================================%% 


%%TODO: Unir este metodo con attemp_transfer 
%% move cars on transfer success
keep_moving(CarsQueque, ObsPosition, LogData) ->
    io:format("keep moving CarsQueque ~w, ObsPosition ~w~n",[CarsQueque, ObsPosition]),
    write_result(LogData, io_lib:format("keep moving CarsQueque ~w, ObsPosition ~w~n",[CarsQueque, ObsPosition])),
    keep_moving(CarsQueque, ObsPosition, -1, [], LogData).

keep_moving([], _ObsPosition, _LastPosition, UpdatedCars, LogData)->
    io:format("STOP keep moving UpdatedCars ~w~n",[UpdatedCars]),
    write_result(LogData, io_lib:format("STOP keep moving UpdatedCars ~w~n",[UpdatedCars])),
    lists:reverse(UpdatedCars);

keep_moving([{car,{Wait,Delay, Position, Route, PrefLanes}}|Tail], ObsPosition,LastPosition, UpdatedCars, 
  LogData) when Position - 1 /= ObsPosition, Position - 1 /= LastPosition ->
    NewPosition = Position - 1,
    io:format("keep moving OK NewPosition ~w. Tail ~w~n",[NewPosition, Tail]),
    write_result(LogData, io_lib:format("keep moving OK NewPosition ~w. Tail ~w~n",[NewPosition, Tail])),
    keep_moving(Tail, ObsPosition, NewPosition, [{car,{Wait + 1,Delay, NewPosition, Route, PrefLanes}} |  UpdatedCars],LogData);
    
keep_moving([{car,{Wait,Delay, Position, Route, PrefLanes}}|Tail], ObsPosition,LastPosition, UpdatedCars,
  LogData) when Position - 1 == ObsPosition; Position - 1 == LastPosition ->
    io:format("keep moving NO MORE~n",[]),
    write_result(LogData, io_lib:format("keep moving NO MORE~n",[])),
    keep_moving(Tail, ObsPosition, Position,[{car,{Wait + 1,Delay, Position, Route, PrefLanes}} |  UpdatedCars], LogData).

%%Stop cars from moving after an obstacle has been found or when the red light has been given
stop_moving([{car,{Wait,Delay, Position, Route, PrefLanes}}|Waiting]) ->
    io:format("STOPING CARS~n",[]),
    stop_moving(Waiting, Position, [{car,{Wait,Delay, Position, Route, PrefLanes}} | []]).
stop_moving(CarsQueque, LastPosition) ->
    io:format("STOPING CARS tow args~n",[]),
    stop_moving(CarsQueque, LastPosition, []).
stop_moving([], _LastPosition, UpdatedCars) -> 
    io:format("ALL CARS STOPED~n",[]),
    lists:reverse(UpdatedCars);
stop_moving([{car,{Wait,Delay, Position, Route, PrefLanes}}|Waiting], LastPosition, UpdatedCars) when LastPosition < Position -1 ->
    io:format("CAR CAN MOVE~n",[]),
    stop_moving( Waiting, Position - 1, [{car,{Wait + 1,Delay, Position - 1, Route, PrefLanes}} | UpdatedCars]);
stop_moving([{car,{Wait,Delay, Position, Route, PrefLanes}}|Waiting], LastPosition, UpdatedCars) when LastPosition >= Position -1 ->
    NewUpdated = [{car,{Wait + 1,Delay + 1, Position, Route, PrefLanes}} | UpdatedCars],
    io:format("CAR CAN NOt MOVE waiting: ~w ... Position ~w... NewUpdated: ~w ~n",[Waiting, Position, NewUpdated]),
    stop_moving( Waiting, Position, NewUpdated).


%% Get the las car on the list
get_lastPosition([]) -> {car,{0,0,-1,[],[]}};
get_lastPosition(CarsQueque) -> lists:last(CarsQueque).

%% Add cars to selected lanelane:
%% function used in estimate arrival if car entered goes to respective lane, 
%% if not enters the waiting list of the respective source lane
%% at any case, update times for cars lists
add_car({LaneId, LanePid, WaitingOutside, _ProbData, _Timer}, Car, LogPath) ->    
    %%NewArrival  =  new_arrival({LaneId, LanePid,ProbData}),
    io:format("Car to add ~w~n",[Car]),
    LanePid ! {incoming, self(), Car},    
    receive
        {reply, full}  ->
            io:format("Lane ~w its full. Adding to wait list~n",[LaneId]),
            write_result(LogPath, io_lib:format("Lane ~w its full. Adding to wait list",[LaneId])), 
            WaitingUpdated = waiting([Car|WaitingOutside], LogPath),
            WaitingUpdated;
            %%{[{LaneId, LanePid, WaitingUpdated, ProbData, NewArrival} | SourcesLane], NewArrival};
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            write_result(LogPath, io_lib:format("Card Added to lane ~w",[LaneId])),   
            WaitingUpdated = waiting(WaitingOutside, LogPath),
            WaitingUpdated
            %%{[{LaneId, LanePid, WaitingUpdated, ProbData, NewArrival} | SourcesLane], NewArrival}
    end.

%%=================================================================================================================================
%%=================================================================================================================================
%%=================================================================================================================================
%%=================================================================================================================================

%%  ____   ___    ____    ____ 
%% |    | |   |  |    |  |    |      /\    
%% |____| |___|  |    |  |____|_    /__\   
%% |      |  \   |    |  |      |  /    \  
%% |      |   \  |____|  |______| /      \ 
%%

%% Estimate arrival for new cars to the zone
estimate_new_arrival(SourcesLane, LogPath) ->
    %%_Prob = random:uniform(),
    estimate_new_arrival(SourcesLane,[], LogPath).
estimate_new_arrival([], SourcesLane, _LogPath) ->
    io:format("Ending ~w ~n",[SourcesLane]),
    SourcesLane;
    
%% when the waiting list of the source lane is empty, check for the probability 
%% if the current time is when a car hast to arrive then try to add it to the lane with add_car function
%% as a result we get UpdatedSources to pass it ass a parameter
estimate_new_arrival([{LaneId, LanePid, [], ProbData, 0}| Tail], SourcesLane, LogPath) ->
    io:format("ADDING CAR TO LANE FROM EMPTY OUTSIDE SOURCE ~n",[]),
    write_result(LogPath, io_lib:format("ADDING CAR TO LANE FROM EMPTY OUTSIDE SOURCE. LANE ~w ~n",[LaneId])),
    Car = {car,{0,0,-1,[],[]}},
    %%{UpdatedSources, NewArrival} = add_car({LaneId, LanePid, [],  ProbData, 0}, Car, SourcesLane, LogPath),
    WaitingUpdated = add_car({LaneId, LanePid, [],  ProbData, 0}, Car, LogPath),
    NewArrival  =  new_arrival({LaneId, LanePid,ProbData}),
    CurrentSource = {LaneId, LanePid, WaitingUpdated, ProbData, NewArrival}, 
    case NewArrival of
        0 -> %%[CurrentSource | UpdatedSourcesTail] = UpdatedSources,
             estimate_new_arrival([CurrentSource | Tail], SourcesLane, LogPath);
        _ -> estimate_new_arrival(Tail, [CurrentSource | SourcesLane], LogPath)
    end;     
    

%% In the case that an arrival is expected and there are cars waiting outside, get the first car waiting in the queque
%% try to send it to the source lane if succeded remove it from waiting list and add a new car to the same
estimate_new_arrival([{LaneId, LanePid, [WaitingCar|WaitingOutside], ProbData, 0}| Tail], SourcesLane, LogPath) ->
    io:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE ~n",[]),
    write_result(LogPath, io_lib:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE. LANE ~w ~n",[LaneId])),
    NewWaiting = lists:reverse([{car,{0,0,-1,[],[]}} | lists:reverse(WaitingOutside)]),    
    %%{UpdatedSources, NewArrival} = add_car({LaneId, LanePid, NewWaiting, ProbData, 0}, WaitingCar, SourcesLane, LogPath),
    WaitingUpdated = add_car({LaneId, LanePid, NewWaiting,  ProbData, 0}, WaitingCar, LogPath),
    NewArrival  =  new_arrival({LaneId, LanePid,ProbData}),
    CurrentSource = {LaneId, LanePid, WaitingUpdated, ProbData, NewArrival}, 
    case NewArrival of
        0 -> %%[CurrentSource | UpdatedSourcesTail] = UpdatedSources,
             estimate_new_arrival([CurrentSource | Tail], SourcesLane, LogPath);
        _ -> estimate_new_arrival(Tail, [CurrentSource | SourcesLane], LogPath)
    end;

%% If there is still missing time for a car to arrive, just update times for all cars on waiting lists
%% and try to send the first car waiting outside to the lane
estimate_new_arrival([{LaneId, LanePid, [], ProbData, Timer}| Tail], SourcesLane, LogPath) ->
    io:format("NOT ADDING CAR TO LANE ~n",[]),
    write_result(LogPath, io_lib:format("NOT ADDING CAR TO LANE. LANE ~w ~n",[LaneId])),
    estimate_new_arrival(Tail, [{LaneId, LanePid, [], ProbData, Timer - 1}|SourcesLane], LogPath);


estimate_new_arrival([{LaneId, LanePid, [WaitingCar|WaitingOutside], ProbData, Timer}| Tail], SourcesLane, LogPath) ->
    io:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE OFFSCHEDULE ~n",[]),
    write_result(LogPath, io_lib:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE OFFSCHEDULE. LANE ~w ~n",[LaneId])),
    %%NewWaiting = lists:reverse([{car,{0,0,-1,[],[]}} | lists:reverse(WaitingOutside)]),    
    %%{UpdatedSources, _NewArrival} = add_car({LaneId, LanePid, WaitingOutside, ProbData, Timer}, WaitingCar, SourcesLane, LogPath),
    WaitingUpdated = add_car({LaneId, LanePid, WaitingOutside,  ProbData, Timer}, WaitingCar, LogPath),
    estimate_new_arrival(Tail, [{LaneId, LanePid, WaitingUpdated,  ProbData, Timer - 1} | SourcesLane], LogPath).  

%%estimate_new_arrival([{LaneId, LanePid, WaitingOutside, ProbData, Timer}| Tail], SourcesLane, LogPath) ->
%%    io:format("NOT ADDING CAR TO LANE ~n",[]),
%%    write_result(LogPath, io_lib:format("NOT ADDING CAR TO LANE. LANE ~w ~n",[LaneId])),
%%    WaitingUpdated = waiting(WaitingOutside), 
%%    estimate_new_arrival(Tail, [{LaneId, LanePid, WaitingUpdated, ProbData, Timer - 1}|SourcesLane], LogPath).

%%=======================================================================%%
%%=========================GENERAL FUNCTIONS=============================%%
%%=======================================================================%%

%% Calculate statistics
%% use the module statistics to calculate cars arrival and other scenaries
data_distribution(Server) ->
    Server ! {valor, self()},
    receive
  	{reply, DataValue} -> %io:format("Valor recibido ~w~n",[Arrival]),
    		   	 DataValue;
    	Other	      -> io:format("Error calling the server: ~w~n",[Other])
    end.
%%=======================================================================%%
%%=======================================================================%%
%%=======================================================================%%


   
%%=======================================================================%%
%%=========================BEGIN OF POISSON PROB=========================%%
%%=======================================================================%%

%% Estimate initial Cars arrival for all sources lanes
init_poisson(SourcesLanes) ->
    io:format("LOOKING FOR POISSON ~n",[]),
    init_poisson(SourcesLanes, []).
init_poisson([],SourceUpdated) ->
    io:format("POISSON ENDED ~n",[]),
    SourceUpdated;
init_poisson([{LaneId, LanePid, [], ProbData}| Tail], SourceUpdated) ->
    Arrival = data_distribution(poissonServer),
    io:format("Arrival time ~w~n",[Arrival]),
    NewLane = {LaneId, LanePid, [],ProbData, Arrival},
    init_poisson(Tail, [NewLane | SourceUpdated]).
    
%% Determine new car arrival for lane
new_arrival({_LaneId, _LanePid, _ProbData}) ->
 io:format("New Arrival search~n",[]),
 data_distribution(poissonServer).

%%=======================================================================%%
%%=========================END OF POISSON================================%%
%%=======================================================================%%



%%=======================================================================%%
%%====================BEGIN OF GEOMETRIC DIST============================%%
%%=======================================================================%%

%% Estimate initial Cars arrival for all sources lanes
init_geometric(LanesList) ->
    io:format("LOOKING FOR Geometric ~n",[]),
    init_geometric(LanesList, []).
init_geometric([],LanesUpdated) ->
    io:format("Geometric ENDED ~n",[]),
    LanesUpdated;
init_geometric([{LaneId, LC,Dir, Type, ConnectedLanes, 
                Cars, IsSource, Capacity, ProbData}| Tail], LanesUpdated) when Type == 2 ->
    Turn = {dispatch, data_distribution(geoServer)},
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
    %% get the allow pass prob for each sibling
    AllowPass = {transfer, allow_pass_siblings(List)},
    %%io:format("Turning Car number ~w, AllowPass: ~w~n",[Turn, AllowPass]),
    NewLane = {LaneId, LC,Dir, Type, ConnectedLanes, 
                Cars, IsSource, Capacity, ProbData, [Turn,AllowPass]},
    init_geometric(Tail, [NewLane | LanesUpdated]);
init_geometric([{LaneId, LC,Dir, Type, ConnectedLanes, 
                Cars, IsSource, Capacity, ProbData}| Tail], LanesUpdated) when Type == 1 ->
    Turn = {dispatch, -1},
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
    AllowPass = {transfer, allow_pass_siblings(List)},
    %%io:format("Turning Car number ~w, AllowPass: ~w ~n",[Turn, AllowPass]),
    NewLane = {LaneId, LC,Dir, Type, ConnectedLanes, 
                Cars, IsSource, Capacity, ProbData, [Turn,AllowPass]},
    init_geometric(Tail, [NewLane | LanesUpdated]).

%% Add a prob of allow pass for each sibling on the list
allow_pass_siblings(Siblings) ->
    allow_pass_siblings(Siblings, []).
allow_pass_siblings([], []) ->
    -91;
allow_pass_siblings([], TransferList) ->
    TransferList;
allow_pass_siblings([LaneId | Tail], TransferList) -> 
    allow_pass_siblings(Tail, [{LaneId, data_distribution(geoCedServer)} | TransferList]).

%% Determine new car turn for lane
new_turn() ->
 io:format("New turn search~n",[]),
 data_distribution(geoServer).

%% Determine new allow car transfer (change lane)
new_transfer() ->
  io:format("New transfer search~n",[]),
 data_distribution(geoCedServer).

%%=======================================================================%%
%%====================END OF GEOMETRIC DIST==============================%%
%%=======================================================================%%
 
%% Write down the results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).
    
write_final_data([], Path, LaneId) ->
    write_result(Path, io_lib:format("FINISH WRITEDOWN CARS INFO FOR LANE: ~w",[LaneId])),
    write_result(Path, io_lib:format("=======================================",[]));
write_final_data([Car|Tail], Path, LaneId) ->    
    write_result(Path, io_lib:format(" ~w",[Car])),
    write_final_data(Tail, Path, LaneId).
    
