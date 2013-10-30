-module(lane).

-export([start/2, estimate_new_arrival/2, init_poisson/1, init_geometric/1, connect/2, 
	checkpoint/2, restore/1, restore/4, restore_sources/2, checkpoint_sources/2, safe_sensor_update/4]).

-export([init/2]).

%% Main call function to spawn proccess
start(Source, Args) ->
    spawn(lane,init, [Source, Args]).
    
init(normal, {LaneId, Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData, TopSpeed}) ->    
    %%NEW stats: dsp_str: dispatch to straight lane 
    %%		 dsp_trn: dispatch to turn lane
    %%		 tra_sib: transfers to siblings
    %% all this items are counters
    lane(LaneId, Type, ConnectedLanes, CarsQueque,[], Capacity, Obstruction, ProbData, [{dsp_str,0},{dsp_trn,0},{tra_sib, 0}], TopSpeed);

init(restore, {LaneId, Type, ConnectedLanes, CarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed}) ->
	%% restore form a checkpoint faile
	lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed).

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

lane(LaneId, Type, ConnectedLanes, CarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed) ->
  %% For each lane on every street in the system, 
  %% get the move acording to the ligth state passed on messages
  %% and run the complete simulation
    receive
    %% if a go message is received it tells all cars to start to move
        {go, LightController, _TimeCycle, _Time, LogData, Sensor} ->            
            CedPos = locate_cedPositions(ConnectedLanes, ProbData, LogData),
            io:format("GO msj. CarsQueque LaneId ~w: CedPos: ~w ~w~n",[LaneId, CedPos, {Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}]),
            write_result(LogData, io_lib:format("GO msj . CarsQueque LaneId ~w: CedPos: ~w ~w~n",[LaneId, CedPos, {Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}])), 
	    {NewCarsQueque, NewProbData, NewOutArea, NewStats} = move_cars(CarsQueque, ConnectedLanes,Obstruction, [], Capacity,ProbData, LaneId, OutSideArea, LogData, Stats,-1, CedPos, Sensor),
            io:format("Moving msj received from ~w. NewCarsQueque: ~w ~n",[LightController,NewCarsQueque]),
            
        %timer:apply_after(100, lane, safe_sensor_update, [Sensor, dsp_str, LaneId, 1]),
        %timer:apply_after(100, lane, safe_sensor_update, [Sensor, dsp_trn, LaneId, 1]),
	    reply(LightController, updated),
	    lane(LaneId, Type, ConnectedLanes, NewCarsQueque,NewOutArea, Capacity, Obstruction, NewProbData, NewStats, TopSpeed);
    %% send a message to all cars to start to stop preventing them to pass the red ligth
        {stop, LightController, LogData} ->  	
	    NewCarsQueque = stop_moving(CarsQueque),
	    io:format("Stop msj received from ~w.~n",[LightController]),
	    write_result(LogData, io_lib:format("Stop msj received from ~w",[LightController])),
	    reply(LightController, updatedStop),
	    lane(LaneId, Type, ConnectedLanes, NewCarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
	{waiting, LightController, _Time, LogData} ->
    %% When recieving a waiting message, update cars times if they can or cannot move
	    CedPos = locate_cedPositions(ConnectedLanes, ProbData, LogData),
	    write_result(LogData, io_lib:format("CedPos: ~w ~n",[CedPos])), 
	    NewCarsQueque = waiting(LaneId, CarsQueque, LogData, ConnectedLanes, Obstruction, CedPos),
	    %%io:format("Waiting msj received from ~w. LogData~p~n",[LightController,LogData]),
	    write_result(LogData, io_lib:format("Waiting msj received from ~w",[LightController])),
	    reply(LightController, updatedWaiting),
	    lane(LaneId, Type, ConnectedLanes, NewCarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
    %% connect_lane msg used to update ConnectedLanes list with any type of lane
	{connect_lane, AdjLane, Pid}  ->     		
    	    %%io:format("{Connected Lanes ~w.~n",[ConnectedLanes]),
    	    NewConnectedLanes = [AdjLane| ConnectedLanes],
     	    %%io:format("{new Connected Lanes ~w.~n",[NewConnectedLanes]),
    	    reply(Pid,ok),
    	    lane(LaneId, Type, NewConnectedLanes, CarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
    %% incoming msg, used when a car arrives to lane
	{incoming, Pid, Car, CarLength} ->
	    io:format("Car incoming~n"),    
    %% get cars last position to evaluate if a new car can enter the lane
	    %%LastPosition =  -1,       	    
	    LastPosition = get_lastPosition(CarsQueque),
	    io:format("LastCar Position: ~w~n",[LastPosition]),
    %% in case that lanes capactiy has not been reached and that the last position is free
    %% add the car to the end of the lane, then reply with an ok, if not reply with a full 
    	    LaneLastPos = Capacity - 1,
    	    NoObsBegin = no_obs_on_dispatch(Obstruction, LaneLastPos - CarLength),
    	    
    	    %% determine wether the car can pass or not (false if not space, CarData is there is space)
    	    %% set car to the las position (capacity -1)
    	    NewCarData = available_space(Car, TopSpeed, LastPosition, LaneLastPos), 	    	    
	    %%NewCarData = {CarType,{0,0,Capacity - 1, Route, PrefLanes, NextMove, TopSpeed}},
	    io:format("Car data ~w... Carqueque ~w~n", [NewCarData, CarsQueque]),
    	    
    	    io:format("Lane ~w lastpos ~w, NoObsBegin ~w. ObstructionData: ~w ~n",[LaneId, LaneLastPos,NoObsBegin, Obstruction]),
	    case ((NewCarData /= false) and (length(CarsQueque) < Capacity) and (LastPosition < LaneLastPos) and (NoObsBegin /= false))  of
	        true  ->	            
	            NewCarsQueque = lists:reverse([NewCarData|lists:reverse(CarsQueque)]),
	            reply(Pid, ok),
	            lane(LaneId, Type, ConnectedLanes, NewCarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
	        false when NoObsBegin == false ->
	            {siblings, Siblings} = lists:keyfind(siblings, 1, ConnectedLanes),
	            ResObs = check_siblings_obs(Siblings),
	            reply(Pid, ResObs),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
	        false ->
	            reply(Pid, full),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed)
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
	            %%change stats
	            {tra_sib, Counter} = lists:keyfind(tra_sib, 1, Stats),
	            NewStats = lists:keyreplace(tra_sib,1, Stats, {tra_sib, Counter + 1}),	            
	            io:format("Car transfered. Answering to lane ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[NewProbData,NewLaneCedList,NewCedNumCar] ),
	            write_result(LogData, io_lib:format("Car transfered. Answering to lane ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[NewProbData,NewLaneCedList,NewCedNumCar])),
	            io:format("Lane ~w New CarQueque: ~w~n",[LaneId,NewCarsQueque] ),
	            write_result(LogData, io_lib:format("Lane ~w New CarQueque: ~w~n",[LaneId, NewCarsQueque])),
	            reply(CallerLPid, ok),
	            lane(LaneId, Type, ConnectedLanes, NewCarsQueque,OutSideArea, Capacity, Obstruction, NewProbData, NewStats, TopSpeed);
	        {false, _SameQueque, NewCedNumCar} ->
	            NewLaneCedList = lists:keyreplace(CallerId,1, List, {CallerId, NewCedNumCar}),
	            NewProbData = lists:keyreplace(transfer,1, ProbData, {transfer, NewLaneCedList}),
	            io:format("Lane ~w Same CarQueque: ~w~n",[LaneId, CarsQueque]),
	            write_result(LogData, io_lib:format("Lane ~w Same CarQueque: ~w~n",[LaneId, CarsQueque])),
	            reply(CallerLPid, no_space),
	            io:format("Car NOT transfered. Answering to lane ~w ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[{CallerId, CallerLPid}, NewProbData,NewLaneCedList,NewCedNumCar]),	            
	            write_result(LogData, io_lib:format("Car NOT transfered. Answering to lane ~w ProbData~w ... LaneCed: ~w... CedNum: ~w~n",[{CallerId, CallerLPid}, NewProbData,NewLaneCedList,NewCedNumCar])),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction,NewProbData, Stats, TopSpeed)
	    end;
	   
	{check_for_obs, Pid} ->	    
	    case no_obs_on_dispatch(Obstruction, Capacity - 1)  of
	        true  ->	            	            
	            reply(Pid, ok);        
	        false ->
	            reply(Pid, obs_on_begin)            
	    end,
	    lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
	
	{siblingLookup, Pid, AltLaneId, LogData} ->
	    Res = sibling_look_up([main, secondary], ConnectedLanes, AltLaneId),
	    write_result(LogData, io_lib:format("RES after looking up for right sibling on dispatch ~w",[Res])),
	    reply(Pid, Res),
	    lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
	
	{yield, Pid, LogData} ->
	    Res = gen_cedPositions(Obstruction, CarsQueque, LogData),
	    reply(Pid, Res),
	    lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed);
	    
	{write_down, Pid, Path,LaneIdS} ->
	    write_result(Path, io_lib:format("=======================================",[])),
	    write_result(Path, io_lib:format("START WRITEDOWN CARS INFO FOR LANE: ~w",[LaneIdS])),
	    write_result(Path, io_lib:format("--------------Cars on lane-------------",[])),
	    write_final_data(CarsQueque, Path),
	    write_result(Path, io_lib:format("--------------Cars on lane-------------",[])),
	    write_result(Path, io_lib:format("-----Cars that leaved area on this lane-----",[])),
	    write_final_data(OutSideArea, Path),	    
	    write_result(Path, io_lib:format("-----Cars that leaved area on this lane-----",[])),
	    write_final_stats(Stats, Path),
	    write_result(Path, io_lib:format("FINISH WRITEDOWN CARS INFO FOR LANE: ~w",[LaneId])),
	    write_result(Path, io_lib:format("=======================================",[])),
	    reply(Pid, finished);
	 {checkpoint, Pid, {LightId, Dir, {ChkPath, CarChkPath, OCarChkPath}}} ->
	 	io:format("Lane ~w: checkpoint called.", [LaneId]), 
	 	write_checkpoint(LaneId,LightId, Dir, Type, ConnectedLanes, CarsQueque, 
	 		OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed, {ChkPath, CarChkPath,OCarChkPath}),
	 	reply(Pid, chkpoint_saved),
	 	lane(LaneId, Type, ConnectedLanes, CarsQueque, OutSideArea, Capacity, 
	 		Obstruction, ProbData, Stats, TopSpeed);
	 {restore, {NCarsQueque, NOutSideArea, NCapacity, NObstruction, NProbData, NStats, NTopSpeed}} ->
	 	lane(LaneId, Type, ConnectedLanes, NCarsQueque, NOutSideArea, NCapacity, 
	 		NObstruction, NProbData, NStats, NTopSpeed);
	 {info, CallerPid} ->
	 	reply(CallerPid, {LaneId, Type,Capacity, Obstruction, TopSpeed}),
	 	lane(LaneId, Type, ConnectedLanes, CarsQueque, OutSideArea, Capacity, 
	 		Obstruction, ProbData, Stats, TopSpeed);
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
waiting([{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], LogData) -> 
    waiting([{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], [], EPos, LogData).
waiting([], UpdatedCars, _LastPosition, LogData) -> 
    NewUpdatedCars =  lists:reverse(UpdatedCars),
    io:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars]),    
    write_result(LogData, io_lib:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars])),
    NewUpdatedCars;
%% update cars that are waiting outsite of sources lanes
waiting([{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], UpdatedCars, _LastPosition, 
  LogData) when BPos == -1-> 
    %%io:format("update cars that are waiting outsite of sources lanes~n",[]),    
    waiting(Tail, [{CarType,{Wait + 1, Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], BPos, LogData).


%% for normal lanes
waiting(_LaneId, [], _LogData, _ConnectedLanes, _Obstructions, _CedPos) ->
    [];
waiting(LaneId, [{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], LogData, ConnectedLanes, Obstructions, CedPos) ->
    waiting(LaneId, [{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], [], EPos, LogData, ConnectedLanes, Obstructions, CedPos).
waiting(_LaneId, [], UpdatedCars, _LastPosition, LogData, _ConnectedLanes, [], _CedPos) -> 
    NewUpdatedCars =  lists:reverse(UpdatedCars),
    io:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars]),    
    write_result(LogData, io_lib:format("Updated waiting reversed ~w, ~n",[NewUpdatedCars])),
    NewUpdatedCars;
%% update cars that are waiting outsite of sources lanes
waiting(LaneId, [{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], UpdatedCars, _LastPosition, _LogData, 
  ConnectedLanes, [], CedPos) when BPos == -1-> 
    %%io:format("update cars that are waiting outsite of sources lanes~n",[]),    
    waiting(LaneId, Tail, [{CarType,{Wait + 1, Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], BPos, _LogData, ConnectedLanes, [], CedPos);  
%% in case that lass position has been reached or if theres a car ahead just update times and do not move the car
waiting(LaneId, [{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], UpdatedCars, LastPosition, _LogData, 
  ConnectedLanes, [], CedPos) when BPos == 0; BPos - 1 == LastPosition -> 
    waiting(LaneId, Tail, [{CarType,{Wait + 1, Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], EPos, _LogData, ConnectedLanes, [], CedPos); 
%% when the next position is greather or equal to 0 and if there is no car update times and move forward
waiting(LaneId, [Car = {CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], UpdatedCars, LastPosition, LogData, 
  ConnectedLanes, [], CedPos) when BPos - 1 >= 0, BPos - 1 /= LastPosition ->
   	write_result(LogData, io_lib:format("Call stop_toced : Car ~w  CedPos: ~w.~n",[Car, CedPos])), 
    IsCedPos = stop_toCed(Car, CedPos),
	%%IsCedPos = stop_toCed({CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, CedPos),
	write_result(LogData, io_lib:format("CEDPos : Car ~w  cedPos ~w. ",[{CarType,{Wait,Delay,{BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, CedPos])), 
	case IsCedPos of
        false ->
			{NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, 1),
		    waiting(LaneId,Tail, [{CarType,{Wait + 1, Delay + 1, {NBPos, NEPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], 
			NEPos, LogData, ConnectedLanes, [], CedPos);
		true ->
			io:format("MOVE-CARS-CEDPos : Car reached a cedPos and has to allow pass ~w. LastPos ~w, CedPositions: ~w~n",[BPos - NextMove, BPos, CedPos]),
			write_result(LogData, io_lib:format("MOVE-CARS-CEDPos : Car reached a cedPos and has to allow pass ~w. LastPos ~w, CedPositions: ~w~n",[BPos - NextMove, BPos, CedPos])), 
			waiting(LaneId, Tail, [{CarType,{Wait + 1, Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], EPos, LogData, ConnectedLanes, [], CedPos)
    end;     

waiting(LaneId, CarsQueque, _UpdatedCars, _LastPosition, LogData, ConnectedLanes, [ObsData | _Obstruction], _CedPos)-> 
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
move_cars([], _ConnectedLanes, _Obstruction, UpdatedCars, _LanCap, ProbData, _LaneId, NewOutArea, LogData, Stats, _LastCarPos,_CedPos, _Sensor) -> 
    io:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars]),
    write_result(LogData, io_lib:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars])),
    {lists:reverse(UpdatedCars), ProbData, NewOutArea, Stats};

%% if car has reached the end of line, dispatch (send) car to one of connected lanes
move_cars([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, Obs, UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData, Stats, _LastCarPos, CedPos, Sensor) when BPos == 0  -> 
    %%Dispatch Cars
    io:format("CAR GOING TO DISPATCH Prob dispatch: ~w.~n",[ProbData]),
    write_result(LogData, io_lib:format("CAR GOING TO DISPATCH Prob dispatch: ~w.~n",[ProbData])),
    {dispatch, TurnCarNum} = lists:keyfind(dispatch, 1, ProbData),
    %%Get parent lane 
    S = atom_to_list(LaneId),
    ParentLaneId = list_to_atom(string:concat(string:substr(S,8,2), string:substr(S,5,3))),
    
    %% try to dispatch car to connected lane and get result of it
    {Res, NewTurnCarNum, Dir} = prepare_car_dispatch({CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, ConnectedLanes, Tail,TurnCarNum, {LaneId, ParentLaneId},LogData),
    NewProbData = lists:keyreplace(dispatch,1, ProbData, {dispatch, NewTurnCarNum}),
    io:format("Dispatch result: ~w.~n",[Res]),
    write_result(LogData, io_lib:format("Dispatch result: ~w.~n",[Res])),
    %% if car was able to move to the next lane, continue with remaining cars, if not stop moving the rest of
    %% the cars
    case Res of 
        {reply, transfered, Car}   -> move_cars(Tail, ConnectedLanes, Obs, UpdatedCars, LanCap,NewProbData, LaneId, [Car|NewOutArea], LogData, Stats,-1, CedPos, Sensor);
        {reply, transfered} when Dir == str ->
        				  %%update the sensor counter by the amount specified
        				  %io:format("Calling sensor ~w for lane: ~w with data: ~w~n", [Sensor, LaneId, {dsp_str, 1}]),
						  %safe_sensor_update(Sensor, dsp_str, LaneId, 1),
        			      timer:apply_after(50, lane, safe_sensor_update, [Sensor, dsp_str, LaneId, 1]),
        			      {dsp_str, StrCounter} = lists:keyfind(dsp_str, 1, Stats),
         			      NewStats = lists:keyreplace(dsp_str,1, Stats, {dsp_str, StrCounter + 1}),         			      
						  %timer:apply_after(100, moduler, update_dm, [NN, NNFile, LightId]),
        			      move_cars(Tail, ConnectedLanes, Obs, UpdatedCars, LanCap,NewProbData, LaneId,NewOutArea, LogData, NewStats,-1, CedPos, Sensor);
        			      
        {reply, transfered} when Dir == trn -> 
        			      {dsp_trn, TrnCounter} = lists:keyfind(dsp_trn, 1, Stats),
         			      NewStats = lists:keyreplace(dsp_trn,1, Stats, {dsp_trn, TrnCounter + 1}),
         			      %%update the sensor counter by the amount specified
         			      %safe_sensor_update(Sensor, dsp_trn, LaneId, 1),
         			      timer:apply_after(50, lane, safe_sensor_update, [Sensor, dsp_trn, LaneId, 1]),
        			      move_cars(Tail, ConnectedLanes, Obs, UpdatedCars, LanCap,NewProbData, LaneId,NewOutArea, LogData, NewStats,-1, CedPos, Sensor);
        			      
        {reply, error, NewUpdated} -> io:format("CALL STOP MOVING ~w  position ~w.~n",[NewUpdated, EPos]),
        			      write_result(LogData, io_lib:format("CALL STOP MOVING ~w  position ~w.~n",[NewUpdated, EPos])),
           			      {stop_moving(NewUpdated, EPos), NewProbData, NewOutArea, Stats}
    end;


%% if its the car cannot move anymore
move_cars([ Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos, CedPos, Sensor) when BPos /= 0 andalso NextMove == 0 andalso (BPos - 1 >= 0 orelse BPos - 1 > LastCarPos) -> 
 	write_result(LogData, io_lib:format("Call stop_toced : Car ~w  CedPos: ~w.~n",[Car, CedPos])), 
    IsCedPos = stop_toCed(Car, CedPos),
 	%%IsCedPos = stop_toCed({CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, CedPos),
	case IsCedPos of
        false ->
			NewNextMove = NextMove + 2,
			%%NewPosition = Position - 1,
			{NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, 1),
			io:format("MOVE-CARS-restart : Car be moved, changed movement from ~w to ~w. LastPos ~w, CarQueque: ~w~n",[NextMove, NextMove + 1, NEPos, Tail]),
			write_result(LogData, io_lib:format("MOVE-CARS-restart : Car be moved, changed movement from ~w to ~w. LastPos ~w, CarQueque: ~w~n",[NextMove, NextMove + 1, NEPos, Tail])), 
			move_cars(Tail, ConnectedLanes,[], [{CarType,{Wait + 1,Delay, {NBPos, NEPos, Length}, Route, PrefLanes, NewNextMove, TopMove}} | UpdatedCars], 
			  LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, NEPos, CedPos, Sensor);
		true ->
			io:format("MOVE-CARS-CEDPos : Car reached a cedPos and has to allow pass ~w. LastPos ~w, CedPositions: ~w~n",[BPos - NextMove, BPos, CedPos]),
			write_result(LogData, io_lib:format("MOVE-CARS-CEDPos : Car reached a cedPos and has to allow pass ~w. LastPos ~w, CedPositions: ~w~n",[BPos - NextMove, BPos, CedPos])), 
			move_cars(Tail, ConnectedLanes,[], [{CarType,{Wait + 1,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], 
			  LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, EPos, CedPos, Sensor)
    end;
      
%% if its the car cannot move anymore
move_cars([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos, CedPos, Sensor) when BPos /= 0 andalso NextMove == 0 andalso (BPos - 1 < 0 orelse BPos - 1 =< LastCarPos) -> 
    io:format("MOVE-CARS-restart : Car cannot be moved, leave movement at ~w. LastPos ~w, CarQueque: ~w~n",[NextMove,BPos, Tail]),
    write_result(LogData, io_lib:format("MOVE-CARS-first : Car cannot be moved, leave movement at ~w. LastPos ~w, CarQueque: ~w~n",[NextMove, BPos, Tail])), 
    move_cars(Tail, ConnectedLanes,[], [{CarType,{Wait + 1,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], 
      LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, EPos, CedPos, Sensor);

%% if its the first car in the line
%%move_cars([{CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
%%  ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos) when Position /= 0,Position - NextMove >= 0, LastCarPos == -1  -> 
%%    NewNextMove = speed_up(NextMove, TopMove),
%%    io:format("MOVE-CARS-first : Car moved ok, change movement from ~w to ~w. LastPos ~w, CarQueque: ~w~n",[NextMove, NewNextMove, Position - NextMove, Tail]),
%%    write_result(LogData, io_lib:format("MOVE-CARS-first : Car moved ok, change movement from ~w to ~w. LastPos ~w, CarQueque: ~w~n",[NextMove, NewNextMove, Position - NextMove, Tail])), 
%%    move_cars(Tail, ConnectedLanes,[], [{CarType,{Wait + 1,Delay, Position - NextMove, Route, PrefLanes, NewNextMove, TopMove}} | UpdatedCars], 
%%      LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, Position - NextMove);

%% if its the first car in the line
%%move_cars([{CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
%%  ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos) when Position /= 0, Position - NextMove < 0, LastCarPos == -1  ->
%%    io:format("MOVE-CARS-first: Reducing movement from ~w to ~w. Try again~n",[NextMove, NextMove - 1]),
%%    write_result(LogData, io_lib:format("MOVE-CARS-first: Reducing movement from ~w to ~w. Try again~n",[NextMove, NextMove - 1])), 
%%    move_cars([{CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove - 1, TopMove}} | Tail], ConnectedLanes,[], UpdatedCars, 
%%      LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos);


%% if its ANY OTHER car in the line
move_cars([ Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos, CedPos, Sensor) when (BPos /= 0 andalso NextMove > 1 andalso BPos - NextMove - 1 >= 0 andalso BPos - NextMove - 1 > LastCarPos)
  orelse (BPos /= 0 andalso NextMove == 1 andalso BPos - NextMove >= 0 andalso BPos - NextMove > LastCarPos)  -> 
  	write_result(LogData, io_lib:format("Call stop_toced : Car ~w  CedPos: ~w.~n",[Car, CedPos])), 
    IsCedPos = stop_toCed(Car, CedPos),
    case IsCedPos of
        false ->
			NewNextMove = speed_up(NextMove, TopMove),
			{NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, NextMove),
			io:format("MOVE-CARS : Car moved ok, change movement from ~w to ~w. ~n",[NextMove, NewNextMove]),
			write_result(LogData, io_lib:format("MOVE-CARS : Car moved ok, change movement from ~w to ~w.~n",[NextMove, NewNextMove])), 
			move_cars(Tail, ConnectedLanes,[], [{CarType,{Wait + 1,Delay, {NBPos, NEPos, Length}, Route, PrefLanes, NewNextMove, TopMove}} | UpdatedCars], 
			  LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, NEPos, CedPos, Sensor);
		true ->
			io:format("MOVE-CARS-CEDPos : Car reached a cedPos and has to allow pass ~w. LastPos ~w, CedPositions: ~w~n",[BPos - NextMove, BPos, CedPos]),
			write_result(LogData, io_lib:format("MOVE-CARS-CEDPos : Car reached a cedPos and has to allow pass ~w. LastPos ~w, CedPositions: ~w~n",[BPos - NextMove, BPos, CedPos])), 
			move_cars(Tail, ConnectedLanes,[], [{CarType,{Wait + 1,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], 
			  LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, EPos, CedPos, Sensor)
    end;

%% if its ANY OTHER car in the line
move_cars([Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos, CedPos, Sensor) when (BPos /= 0 andalso NextMove > 1 andalso (BPos - NextMove - 1 < 0 orelse BPos - NextMove - 1 =< LastCarPos))
  orelse (BPos /= 0 andalso NextMove == 1 andalso (BPos - NextMove < 0 orelse BPos - NextMove =< LastCarPos)) ->
    NewNextMove = NextMove - 1,
    io:format("MOVE-CARS: Car: ~w - lastPos ~w Reducing movement from ~w to ~w. Try again~n",[Car, LastCarPos,NextMove, NewNextMove]),
    write_result(LogData, io_lib:format("MOVE-CARS: Car: ~w - lastPos ~w Reducing movement from ~w to ~w. Try again~n",[Car, LastCarPos,NextMove, NewNextMove])), 
    move_cars([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NewNextMove, TopMove}} | Tail], ConnectedLanes,[], UpdatedCars, 
      LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, LastCarPos, CedPos, Sensor);

 
%% in case that thers and obstruction on the lane
move_cars(CarsQueque, ConnectedLanes, [ObsData | _Obstruction], _UpdatedCars, 
            _LanCap, ProbData, LaneId, NewOutArea, LogData, Stats, _LastCarPos, _CedPos, _Sensor)-> 
%% get probability for the current car to see if its enable to 
%% cross to a sibling lane
%% if it was not able, leave it on the same lane and update times
%% and cotinue with the rest of the list
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
    %%{transfer, CedCarNum} = lists:keyfind(transfer, 1, ProbData),
%% try to move the cars to the a sibling lane
    UpdatedCarsQueque = transfer_enabled(LaneId, List, length(List), CarsQueque, ObsData, LogData),
    %%NewProbData = lists:keyreplace(transfer,1, ProbData, {transfer, NewCedCarNum}),
    {UpdatedCarsQueque,ProbData,NewOutArea, Stats} .

speed_up(CurrentMove, MaxMove) when CurrentMove >= MaxMove ->
    MaxMove;
speed_up(CurrentMove, _MaxMove) ->
    CurrentMove + 1.

stop_toCed(_Car, []) ->
    false;
stop_toCed({_CarType,{_Wait, _Delay, {BPos, EPos, _Length}, _Route, _PrefLanes, NextMove, _TopMove}}, [{_LaneId, CedPos} | _CedPositions])
  when CedPos /= -1 andalso ( ( NextMove > 1 andalso BPos - NextMove - 1 =< CedPos andalso BPos > CedPos andalso EPos > CedPos) orelse 
  (NextMove == 1 andalso BPos - NextMove =< CedPos andalso BPos > CedPos andalso EPos > CedPos) orelse
  (NextMove == 0 andalso BPos - 1 =< CedPos andalso BPos > CedPos andalso EPos > CedPos))  ->
    true;
stop_toCed({CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, [{_LaneId, _CedPos} | CedPositions]) ->
  %%when CedPos /= -1 andalso ( ( NextMove > 1 andalso BPos - NextMove - 1 > CedPos) orelse (NextMove == 1 andalso BPos - NextMove > CedPos) orelse
  %%(NextMove == 0 andalso BPos - 1 > CedPos) )  ->
    stop_toCed({CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, CedPositions).
    
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
no_obs_on_dispatch([{_Obs, _Begin, End} | _Tail], LaneLastPos) when LaneLastPos =< End ->
    false;
no_obs_on_dispatch([_Obs | Tail], LaneLastPos) ->
    no_obs_on_dispatch(Tail, LaneLastPos).

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
    {Res, TurnCarNum, str};
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
    {Res, NewTurnCarNum, str};
prepare_car_dispatch({CarType,{Wait,Delay, Position, Route, PrefLanes, _NextMove, TopMove}}, ConnectedLanes, CarsQueque, 
  TurnCarNum, {LaneId, ParentLaneId},LogData) when TurnCarNum == 0 ->
    %%{Type, List} = lists:keyfind(secondary, 1, ConnectedLanes),
    NewTurnCarNum = new_turn(),  
    ReduceSpeedCar =  {CarType,{Wait,Delay, Position, Route, PrefLanes, 1, TopMove}},
    %%TargetLane = get_enabled_lane([secondary,main], ConnectedLanes, [{LaneId, self()}]),
    TargetLane = get_target_lane([secondary,main], ConnectedLanes, [{LaneId, self()}], ReduceSpeedCar),
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
    {Res, NewTurnCarNum, trn}.

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
get_target_lane(LanesOrder, ConnectedLanes, AuxLane, {CarType,{Wait,Delay, Position, Route, [], NextMove, TopMove}}) ->
    {get_enabled_lane(LanesOrder, ConnectedLanes, AuxLane, Route), {CarType,{Wait,Delay, Position, Route, [], NextMove, TopMove}}};

% in case that the car has a prefered lane use that instead of previous selected (priority: TOP)  
get_target_lane(LanesOrder, ConnectedLanes, AuxLane, {CarType,{Wait,Delay, Position, Route, [TLane | Tail], NextMove, TopMove}}) ->
    case check_lanes_route(Route, [TLane]) of
        true  -> {{prefered, [TLane]}, {CarType,{Wait,Delay, Position, Route, Tail, NextMove, TopMove}}};
        false -> {get_enabled_lane(LanesOrder, ConnectedLanes, AuxLane, Route), {CarType,{Wait,Delay, Position, Route, Tail, NextMove, TopMove}}}
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
check_lanes_route_aux([{PLane, _Wait, _Delay} | _RouteTail], PLane) ->
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
car_dispatch({CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, [{_LaneId, LanePid} | _Tail], _CarsQueque, {_CLaneId,CLanePid, CPLaneId},
  _SList,LogData, TurnCarNum) when LanePid == CLanePid ->
    %%io:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, {CarType,{Wait,Delay, Position, [CPLaneId|Route], PrefLanes, NextMove, TopMove}}]),
    io:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, {CarType,{Wait,Delay, Position, [{CPLaneId,Wait, Delay} |Route], PrefLanes, NextMove, TopMove}}]),
    write_result(LogData, io_lib:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, {CarType,{Wait,Delay, Position, [CPLaneId|Route], PrefLanes, NextMove, TopMove}}])),
    {{reply, transfered, {CarType,{Wait,Delay, Position, [{CPLaneId,Wait, Delay}|Route], PrefLanes, NextMove, TopMove}}}, TurnCarNum - 1};


%%%=========================================================================================================
%%%=======================change if error - TEST TEST TEST TEST TEST========================================

%% if its a differente lane, send an incoming msg to let it now a new car is commingNewTurnCarNum = new_turn(),    
    
car_dispatch(Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, [{LaneId, LanePid} | _Tail], CarsQueque,{CLaneId,CLanePid, CPLaneId},
  SList,LogData, TurnCarNum) when LanePid /= CLanePid ->
    PreSendCar = {CarType,{Wait,Delay, {BPos, EPos, Length}, [{CPLaneId,Wait, Delay} | Route], PrefLanes, NextMove, TopMove}},
    io:format("Lane ~w to dispatch ~w. Car ~w ~n",[{CPLaneId,Wait, Delay}, LaneId, Car]),
    write_result(LogData, io_lib:format("Lane ~w to dispatch ~w. Car ~w ~n",[{CPLaneId,Wait, Delay}, LaneId, Car])),
    LanePid ! {incoming, self(), PreSendCar, Length},
    receive
        %% if a "full" reply is received then return original CarsQueque with the car
        {reply, full}  ->
            io:format("Lane ~w its full on car dispatch. Adding to wait list~n",[LaneId]),
            write_result(LogData, io_lib:format("Lane ~w its full on car dispatch. Adding to wait list~n",[LaneId])),
	    {{reply, error, [{CarType,{Wait + 1,Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|CarsQueque]}, TurnCarNum};
            %%CarsQuequeUpdated = waiting([Car|CarsQueque], []);                    
        {reply, {obs_on_begin, []}} ->
            io:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[]),
            write_result(LogData, io_lib:format("There are no available lanes ahead. Transfer to alt row or avenue taking outside the area~n",[])),
            %% change this and try transfer to sibling
            car_dispatch(Car, [{CLaneId,CLanePid}], CarsQueque, {CLaneId,CLanePid, CPLaneId}, SList, LogData, TurnCarNum);
            
        %% if a "ok" reply is received then return transfered reply
        {reply, {obs_on_begin, List}} ->
            io:format("Lane ~w has an obstruction on the begin of the street. Try to sibling ~w~n",[LaneId, List]),
            write_result(LogData, io_lib:format("Lane ~w has an obstruction on the begin of the street. Try to sibling ~w~n",[LaneId, List])),
            case TurnCarNum == 0 of 
                true ->    %% if its a car turning try to dispatch to other sibling, do not do transfer
                    io:format("The car needed to turn, leaving on the same lane and call dispatch to target's sibling~n",[]),
                    write_result(LogData, io_lib:format("The car needed to turn, leaving on the same lane and call dispatch to target's sibling~n",[])),
                    car_dispatch(Car, List, CarsQueque, {CLaneId,CLanePid, CPLaneId}, SList, LogData, TurnCarNum);
                false ->
                    io:format("The car is not going to turn, calling car_dispatch aux with this targetLanes ~w and this turncarnum ~w ~n",[List, TurnCarNum]),
                    write_result(LogData, io_lib:format("The car is not going to turn, calling car_dispatch aux with this targetLanes ~w and this turncarnum ~w~n",[List, TurnCarNum])),
                    car_dispatch_aux(CLaneId, List,SList, Car, LogData, CarsQueque, TurnCarNum)
            end;                            
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            write_result(LogData, io_lib:format("Card Added to lane ~w~n",[LaneId])),
            {{reply, transfered}, TurnCarNum - 1}
            %%CarsQuequeUpdated = waiting(CarsQueque,[]), 
            
    end.

%% INPUT: Lanes list of to verify for obs on the begining,
%% OUTPUT: {obs_on_begin, ALTERNATIVE LANE TO TAKE}
check_siblings_obs(Siblings) ->
    check_siblings_obs(Siblings,[]).    
check_siblings_obs([], List) ->
    {obs_on_begin, lists:reverse(List)};
check_siblings_obs([{LaneId, LanePid} | Tail], EnabledSiblings) ->
    LanePid ! {check_for_obs, self()},
    receive
        {reply, ok}           -> check_siblings_obs(Tail, [{LaneId, LanePid} | EnabledSiblings]);
        {reply, obs_on_begin} -> check_siblings_obs(Tail, EnabledSiblings) 
    end.


car_dispatch_aux (_CLaneId, [],_SList, {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData, CarsQueque, TurnCarNum) ->
    io:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[]),
    write_result(LogData, io_lib:format("There are no available lanes ahead. Transfer to alt row or avenue~n",[])),
    {{reply, error, [{CarType,{Wait + 1,Delay + 1, Position, Route, PrefLanes, NextMove, TopMove}}|CarsQueque]}, TurnCarNum};                          
car_dispatch_aux (CLaneId,[{AltLaneId, AltLanePid} | SiblingsAlt], SList, Car, LogData, CarsQueque, TurnCarNum) ->
    AltSibLane = get_right_sibling(SList, {AltLaneId, AltLanePid}, LogData),
    Res = try_alt_lane(CLaneId, AltSibLane, Car, LogData, CarsQueque, TurnCarNum),
    case Res of                        
        {reply, error} ->
       		io:format("transfer failed for dispatch to sibling ~w~n",[{AltLaneId, AltLanePid}]),
       	 	write_result(LogData, io_lib:format("transfer failed for dispatch to sibling ~w~n",[{AltLaneId, AltLanePid}])),
       	 	car_dispatch_aux(CLaneId, SiblingsAlt,SList, Car, LogData, CarsQueque, TurnCarNum);
       	 _Other ->
       	 	Res
    end.

%%TRY to send the car to the sibling lane to avoid obs on next street or avenue
%% to get the right sibling, use the SList (siblings list) to determine which one 
%% has the connection to the altLaneId
try_alt_lane(_CLaneId, {error, no_macth}, _Car, _LogData, _CarsQueque, _TurnCarNum) ->
    {reply, error};    
try_alt_lane(CLaneId, AltSibLane = {AltLaneId, AltLanePid}, {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData,_CarsQueque, TurnCarNum) ->
    CarWPrefered = {CarType,{Wait,Delay, Position, Route, [AltSibLane | PrefLanes], NextMove, TopMove}},
    Res = attemp_transfer(CLaneId, {AltLaneId, AltLanePid}, CarWPrefered, LogData),  
    case Res of 
        {ok, nothing} -> {{reply, transfered}, TurnCarNum - 1};
        {no_space, _NewCarData} -> {reply, error}
    end.

%%%=======================change if error - TEST TEST TEST TEST TEST========================================
%%%=========================================================================================================
    
    
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

%%%%%%==============================TEST TEST TEST TEST ============================================================================
transfer_enabled(CurrentLaneId, List, SiblingsNum, CarsQueque, ObsData, LogData) when SiblingsNum == 1 ->
    %%[Sibling | _Tail] = List,
    io:format("Attemp with just one sibling ~w~n",[{List, CarsQueque, [], ObsData}]),
    write_result(LogData, io_lib:format("Attemp with just one sibling ~w~n",[{List, CarsQueque, [], ObsData}])),
    %%attemp_transfer(Sibling, CarsQueque, [], End, CedCarNum);
    attemp_transfer(CurrentLaneId, List, CarsQueque, [], ObsData, LogData, -1);
transfer_enabled(CurrentLaneId, List, SiblingsNum, CarsQueque, ObsData, LogData) when SiblingsNum > 1 ->
    TransferSibling = random:uniform(SiblingsNum),    
    {SiblingId, SiblingPid} = lists:nth(TransferSibling, List),
    NewList = lists:keydelete(SiblingId, 1, List),
    io:format("Attemp with two sibling~n",[]),
    write_result(LogData, io_lib:format("Attemp with two sibling~n",[])),
    attemp_transfer(CurrentLaneId, [{SiblingId, SiblingPid} | NewList], CarsQueque, [], ObsData, LogData, -1);
transfer_enabled(_CurrentLaneId, List, SiblingsNum, CarsQueque, ObsData, LogData) ->
    io:format("You're soo fucked up ~w~n",[{List,SiblingsNum, CarsQueque, [], ObsData}]),
    write_result(LogData, io_lib:format("You're soo fucked up ~w~n",[{List,SiblingsNum, CarsQueque, [], ObsData}])),
    CarsQueque.


%% DONETODO:HACER que el carro trate de pasarse si puede cambia de linea si no, avanza siempre y cuando no haya obstrucion u otro carro
%% Attemp to transfer each car on the lane to sibling line    
%% REMOVED: CedNumCar, because sibling lane has the CedNumtransfer for this lane
attemp_transfer(CurrentLaneId, {SiblingId, SiblingPid}, {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData) ->
    %% when there is an obstacule at the begining of the next lane selected for dispatch
    io:format("CALLING SIBLING FOR TRANSFER on ~w to ~w ~n after dispatch error",[CurrentLaneId, {SiblingId, SiblingPid}]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER on ~w to ~w ~n after dispatch error",[CurrentLaneId, {SiblingId, SiblingPid}])),    
    %%change before for atPoint because it needs to allow pass at some point
    SiblingPid ! {try_transfer,atPoint, {CurrentLaneId, self()},Position, {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData},
    receive
       {reply, ok}    -> io:format("transfer succeded for dispatch ~w~n",[{SiblingId, SiblingPid}]),
       			 write_result(LogData, io_lib:format("transfer succeded for dispatch ~w~n",[{SiblingId, SiblingPid}])),
                         {ok, nothing};
       {reply, no_space} -> io:format("transfer failed for dispatch~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed for dispatch~n",[])),
       		         {no_space, {CarType,{Wait + 1,Delay + 1, Position, Route, PrefLanes, NextMove, TopMove}}}
    end.



attemp_transfer(_CurrentLaneId, _Sibling, [], UpdatedCars, _ObsData, _LogData, _LastPosition) ->
    lists:reverse(UpdatedCars);
%% if the car has reached the obstruction try to transfer, if "ok" move the rest of cars, if not test with other cars
attemp_transfer(CurrentLaneId, SiblingList, [Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | Tail], 
  UpdatedCars, {Obs,ObsBeginPosition, ObsEndPosition}, LogData, _LastPosition) when BPos -1 == ObsEndPosition->
    %% When they reached the obstacule stop the cars on the lane and just update times
    io:format("CALLING SIBLING FOR TRANSFER when position reached on ~w to ~w ~n",[CurrentLaneId, SiblingList]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER when position reached on ~w to ~w ~n",[CurrentLaneId, SiblingList])),
    case attemp_transfer_options({CurrentLaneId, self()}, SiblingList, Car, LogData, atPoint) of
        {reply, ok}    -> attemp_transfer(CurrentLaneId, SiblingList, Tail, UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData, -1);
        {reply, no_space} ->        		  
        	          attemp_transfer(CurrentLaneId, SiblingList, Tail, [{CarType,{Wait + 1,Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], 
        	          	{Obs, ObsBeginPosition, ObsEndPosition}, LogData,EPos) 
    end;
    

attemp_transfer(CurrentLaneId, SiblingList, [Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | Tail], 
  UpdatedCars, ObsData = {Obs, ObsBeginPosition, ObsEndPosition}, LogData, LastPosition) when BPos - 1 > ObsEndPosition ->
    %% when they haven't reached the obstacule try to pass the car to the sibling lane if possible pass if not continue moving
    %% until it reaches the obstacle
    io:format("CALLING SIBLING FOR TRANSFER when position NOT reached on ~w to ~w ~n",[CurrentLaneId, SiblingList]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER when position NOT reached on ~w to ~w ~n",[CurrentLaneId, SiblingList])),
    
    case attemp_transfer_options({CurrentLaneId, self()}, SiblingList, Car, LogData, before) of
       {reply, ok}    -> io:format("transfer succeded after attemps ~w~n",[{SiblingList, Tail, UpdatedCars, ObsData}]),
       			 write_result(LogData, io_lib:format("transfer succeded after attemps ~w~n",[{SiblingList, Tail, UpdatedCars, ObsData}])),
                         attemp_transfer(CurrentLaneId, SiblingList, Tail, UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData, -1);
       {reply, no_space} when BPos -1 >= 0, BPos - 1 > LastPosition -> 
        		 io:format("transfer failed after attemps, car moved on same lane~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed after attemps, car moved on same lane~n",[])),
       			 %%NewPosition = Position - 1,
       			 {NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, 1),
       		         attemp_transfer(CurrentLaneId, SiblingList, Tail, [{CarType,{Wait + 1,Delay, {NBPos, NEPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,NEPos); 
       {reply, no_space} -> 
       			 io:format("transfer failed after attemps, car had to stop~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed after attemps, car had to stop~n",[])),
       		         attemp_transfer(CurrentLaneId, SiblingList, Tail, [{CarType,{Wait + 1,Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,EPos)
    end;
    
attemp_transfer(CurrentLaneId, SiblingList, [{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | Tail], 
  UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData,LastPosition) when BPos - 1 < ObsBeginPosition, BPos - 1 >= 0, BPos - 1 > LastPosition ->
    %% when they aren't in the way of the obstacule do not try to pass the car to the sibling lane 
    io:format("MOVING NORMAL, NOT CALLING SIBLING FOR TRANSFER On ~w ~n",[CurrentLaneId]),
    write_result(LogData, io_lib:format("MOVING NORMAL, NOT CALLING SIBLING FOR TRANSFER On ~w ~n",[CurrentLaneId])),    
    %%NewPosition = Position - 1,
    {NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, 1),
    attemp_transfer(CurrentLaneId, SiblingList, Tail, [{CarType,{Wait + 1,Delay, {NBPos, NEPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,NEPos);
    
attemp_transfer(CurrentLaneId, SiblingList, [{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | Tail], 
  UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData,LastPosition) when BPos - 1 < ObsBeginPosition, BPos - 1 < 0; BPos - 1 == LastPosition ->
    %% when they aren't in the way of thatPointe obstacule do not try to pass the car to the sibling lane 
    io:format("CANT MOVE NORMAL, NOT CALLING SIBLING FOR TRANSFER, NEXT POSITION: ~w IS OCCUPIEDED On ~w ~n",[{BPos - 1, LastPosition}, CurrentLaneId]),
    write_result(LogData, io_lib:format("CANT MOVE NORMAL, NOT CALLING SIBLING FOR TRANSFER, NEXT POSITION: ~w IS OCCUPIEDED On ~w ~n",[{BPos - 1, LastPosition}, CurrentLaneId])),    
    attemp_transfer(CurrentLaneId, SiblingList, Tail, [{CarType,{Wait + 1,Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars], {Obs, ObsBeginPosition, ObsEndPosition}, LogData,EPos);

attemp_transfer(CurrentLaneId, _SiblingList, [{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | Tail], 
  _UpdatedCars, {Obs, ObsBeginPosition, ObsEndPosition}, LogData, _LastPosition) when BPos >= ObsBeginPosition, BPos  =< ObsEndPosition ->
    %% when they aren't in the way of the obstacule do not try to pass the car to the sibling lane 
    io:format("LOCATION ERROR CAR POSITION : ~w IS ON OBSTRUCTION LOCATION: ~w IS OCCUPIEDED On ~w ~n",[BPos, {Obs, ObsBeginPosition, ObsEndPosition}, CurrentLaneId]),
    write_result(LogData, io_lib:format("LOCATION ERROR CAR POSITION : ~w IS ON OBSTRUCTION LOCATION: ~w IS OCCUPIEDED On ~w ~n",[BPos, {Obs, ObsBeginPosition, ObsEndPosition}, CurrentLaneId])),    
    [{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | Tail].
    

attemp_transfer_options(_CurrentLaneId, [], _Car, _LogData, _TransferPoint) ->
    {reply, no_space};
attemp_transfer_options({CurrentLaneId, CLanePid}, [{SiblingId, SiblingPid} | SiblingsAlt], {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData, TransferPoint) ->
    io:format("CALLING SIBLING FOR TRANSFER on ~w to ~w ~n",[CurrentLaneId, {SiblingId, SiblingPid}]),
    write_result(LogData, io_lib:format("CALLING SIBLING FOR TRANSFER on ~w to ~w ~n",[CurrentLaneId, {SiblingId, SiblingPid}])),    
    SiblingPid ! {try_transfer,TransferPoint, {CurrentLaneId, CLanePid},Position, {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData},
    receive
       {reply, ok}    -> io:format("transfer succeded with two sibling ~w~n",[{{SiblingId, SiblingPid}, SiblingsAlt}]),
       			 write_result(LogData, io_lib:format("transfer succeded with two sibling ~w~n",[{{SiblingId, SiblingPid}, SiblingsAlt}])),
                         {reply, ok};
       {reply, no_space} -> 
                         io:format("transfer failed with two sibling~n",[]),
       			 write_result(LogData, io_lib:format("transfer failed with two sibling~n",[])),
       			 attemp_transfer_options({CurrentLaneId, CLanePid}, SiblingsAlt, {CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, LogData, TransferPoint)
    end.

%%%%%%==============================TEST TEST TEST TEST ============================================================================


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


is_space_enabled({CarTypeL,{WaitL, DelayL, {BPosL, EPosL, LengthL},RouteL, PrefLanesL, NextMoveL, TopMoveL}}, [], 
  {BPosT, EPosT, _LengthT}, ProbData, CaToTransfer, UpdatedCars, Capacity, _TransferTime, LogData) 
  when EPosL < BPosT, EPosT =< Capacity ->
    %%CUANDO EL UNICO CARRO EN LA LINEA o el ultimo carro SE ENCUENTRA ANTES DE LA POSICION A PASAR
    io:format("Transfer car after last or only remaining car on lane~n"),
    write_result(LogData, io_lib:format("Transfer car after last or only remaining car on lane~n",[])),
    NewUpdated = [{CarTypeL,{WaitL, DelayL, {BPosL, EPosL, LengthL}, RouteL, PrefLanesL, NextMoveL, TopMoveL}} | UpdatedCars],
    AddCarList = [CaToTransfer | NewUpdated],
    io:format("Transfer car after last. CarList and probdata ~w~n",[{AddCarList, ProbData}]),
    write_result(LogData, io_lib:format("Transfer car after last. CarList and probdata ~w~n",[{AddCarList, ProbData}])),
    {true, lists:reverse(AddCarList), ProbData};

is_space_enabled({CarTypeL,{WaitL, DelayL, PositionL = {BPosL, EPosL, LengthL}, RouteL, PrefLanesL, NextMoveL, TopMoveL}}, CarsQueque, 
  TransferPosition = {BPosT, EPosT, LengthT},ProbData, CaToTransfer, UpdatedCars, _Capacity, TransferTime, LogData) 
  when BPosL > EPosT ->
    %%CUANDO EL PRIMER CARRO EN LA LINEA SE ENCUENTRA DESPUÉS DE LA POSICION A PASAR
    io:format("Transfer car before first car or only remaining car on lane~n"),
    write_result(LogData, io_lib:format("Transfer car before first car or only remaining car on lane~n",[])),
    LocationRes = in_range({BPosL, EPosL, LengthL}, {BPosT, EPosT, LengthT}),
    io:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes]),
    write_result(LogData, io_lib:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes])),
    evaluate_location_result(LocationRes,{CarTypeL,{WaitL, DelayL, PositionL, RouteL, PrefLanesL, NextMoveL, TopMoveL}},UpdatedCars, CaToTransfer,
        CarsQueque, ProbData,  TransferTime);

is_space_enabled({CarTypeL,{WaitL, DelayL, PositionL, RouteL, PrefLanesL, NextMoveL, TopMoveL}}, [], TransferPosition,ProbData, 
  CaToTransfer, UpdatedCars, _Capacity, TransferTime, LogData) ->
    io:format("Transfer car is on the same position of the other car on lane~n"),
    write_result(LogData, io_lib:format("Transfer car is on the same position of the other car on lane~n",[])),
    LocationRes = in_range(PositionL, TransferPosition),
    io:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes]),
    write_result(LogData, io_lib:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, TransferPosition}, LocationRes])),
    evaluate_location_result(LocationRes,{CarTypeL,{WaitL, DelayL, PositionL, RouteL, PrefLanesL, NextMoveL, TopMoveL}},UpdatedCars, CaToTransfer,
        [], ProbData,  TransferTime);

is_space_enabled({CarTypeL,{WaitL, DelayL, PositionL, RouteL, PrefLanesL, NextMoveL, TopMoveL}}, CarsQueque = [{CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}} | CarsTail], TransferPosition,ProbData, 
  CaToTransfer, UpdatedCars, Capacity, TransferTime, LogData) ->
    %%CUANDO EL PRIMER CARRO EN LA LINEA SE ENCUENTRA DESPUÉS DE LA POSICION A PASAR
    io:format("Transfer car between two cars or to the end~n"),
    write_result(LogData, io_lib:format("Transfer car between two cars or to the end~n",[])),
    LocationRes = in_range(PositionL,Position,  TransferPosition),
    io:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, Position, TransferPosition}, LocationRes]),
    write_result(LogData, io_lib:format("Car location: ~w result on in_range function: ~w~n",[{PositionL, Position, TransferPosition}, LocationRes])),
    case LocationRes of
        {false, no_in_range, _Location} ->
           is_space_enabled({CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}, CarsTail, 
              TransferPosition, ProbData, CaToTransfer, [{CarTypeL,{WaitL, DelayL, PositionL, RouteL, PrefLanesL, NextMoveL, TopMoveL}}|UpdatedCars], Capacity, TransferTime, LogData);    
        Other -> 
           evaluate_location_result(Other,{CarTypeL,{WaitL, DelayL, PositionL, RouteL, PrefLanesL, NextMoveL, TopMoveL}},UpdatedCars, CaToTransfer,
              CarsQueque, ProbData,  TransferTime)
    end.


%%IN CASE OF ERROR REMOVE FROM HERE

%% If the diference of distance is the above 2 add the first car and them the new car   
evaluate_location_result ({true, Diff, at_first}, FirstCar, UpdatedCars, 
  CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff >= 2  ->
    NewUpdated = [CaToTransfer | UpdatedCars],
    AddCarList = [FirstCar | NewUpdated],
    {true, lists:append(lists:reverse(AddCarList), CarsQueque), ProbData};

%% if the diferences is not 2 or more, but its time to cd the pass then add the first car, and after that 
%% add the new one
evaluate_location_result ({true, Diff, at_first}, FirstCar, UpdatedCars, 
  CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff < 2, ProbData == 0  ->
    NewUpdated = [CaToTransfer | UpdatedCars],
    AddCarList = [ FirstCar | NewUpdated],
    NewAllowPass = new_transfer(),
    {true, lists:append(lists:reverse(AddCarList), CarsQueque), NewAllowPass};

%% TO HERE
    
    
%% If the diference of distance is the above 2 add the first car and them the new car   
evaluate_location_result ({true, Diff, _Location}, FirstCar, UpdatedCars, 
  CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff >= 2  ->
    NewUpdated = [FirstCar | UpdatedCars],
    AddCarList = [CaToTransfer | NewUpdated],
    {true, lists:append(lists:reverse(AddCarList), CarsQueque), ProbData};

%% if the diferences is not 2 or more, but its time to cd the pass then add the first car, and after that 
%% add the new one
evaluate_location_result ({true, Diff, _Location}, FirstCar, UpdatedCars, 
  CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff < 2, ProbData == 0  ->
    NewUpdated = [FirstCar | UpdatedCars],
    AddCarList = [CaToTransfer | NewUpdated],
    NewAllowPass = new_transfer(),
    {true, lists:append(lists:reverse(AddCarList), CarsQueque), NewAllowPass};

%%used to reduce the allow pass counter (transfer) only when transfer was asked on the position before of a obs
evaluate_location_result ({true, Diff, _Location}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when Diff < 2, ProbData > 0, TransferTime == atPoint ->          
    NewUpdated = [FirstCar | UpdatedCars],
    {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData - 1};

%%used to NOT reduce the allow pass counter (transfer) only when transfer was asked way before the obs
evaluate_location_result ({true, Diff, _Location}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when Diff < 2, ProbData > 0, TransferTime == before ->          
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData};

%% catch for errors
evaluate_location_result ({true, Diff, _Location}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, _TransferTime) when Diff < 2, ProbData < 0 ->          
     io:format("ERROR ON PROBDATA"),
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData};
    
%% In case that there is no space and the transfer was at the point but it
%% was not posible, then reduce the allow pass counter
evaluate_location_result ({false, no_space, _Location}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when TransferTime == atPoint, ProbData > 0 -> %% don't move cars to next position on same lane
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData - 1};

%% If there was a car at the position do not reduce the allow pass counter
evaluate_location_result ({false, no_space, _Location}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when TransferTime == atPoint, ProbData == 0 -> %% don't move cars to next position on same lane
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData};

%% If there was a car at the position and this
evaluate_location_result ({false, no_space, _Location}, FirstCar, UpdatedCars, 
  _CaToTransfer, CarsQueque, ProbData, TransferTime) when TransferTime == before -> %% move cars to next position on same lane
     NewUpdated = [FirstCar | UpdatedCars],
     {false, lists:append(lists:reverse(NewUpdated), CarsQueque), ProbData}.

%%TODO: REVISAR PARA EL CASO DE EL ULTIMO CARRO EN LA FILA Y EL CARRO POR AGREGAR    
%% Check if TransferPosition(Match) is located between the current position of first car (Min)
%% and the next one (Max).
%% ADDED: dummy element on tuple none
in_range({_BPosM, EPosM, _LengthM}, {BPosMX, _EPosMX, _LengthMX}, {BPosMT, EPosMT, _LengthMT}) 
  when BPosMT > EPosM, EPosMT < BPosMX->
    {true, BPosMX - EPosMT, dummy};
in_range({_BPosM, EPosM, _LengthM}, {_BPosMX, EPosMX, _LengthMX}, {BPosMT, _EPosMT, _LengthMT}) 
  when BPosMT > EPosM, BPosMT > EPosMX->
    {false, no_in_range, dummy};
in_range({BPosM, EPosM, _LengthM}, {BPosMX, EPosMX, _LengthMX}, {BPosMT, EPosMT, _LengthMT}) 
  when (BPosMT >= EPosM andalso EPosMT >= BPosMX) orelse (BPosMT >= BPosM andalso EPosMT >= EPosM)
  orelse (BPosMT >= BPosM andalso EPosMT =< EPosM) orelse (BPosMT >= BPosMX andalso EPosMT =< EPosMX) orelse
  ( BPosMT =< EPosM andalso EPosMT == BPosM)->
    {false, no_space, dummy};
in_range(_Min, _Max, _Match) -> 
    {false, -1, dummy}. 
in_range({BPosM, _EPosM, _LengthM}, {_BPosMT, EPosMT, _LengthMT}) when EPosMT < BPosM->
    {true, BPosM - EPosMT, at_first};
in_range({BPosM, EPosM, _LengthM}, {BPosMT, EPosMT, _LengthMT}) 
  when BPosMT == BPosM orelse (BPosMT < BPosM andalso EPosMT >= BPosM) orelse 
  (BPosMT > BPosM andalso BPosMT =< EPosM) ->
    {false, no_space, dummy};
in_range(_Min,_Match) -> 
    {false, -1, dummy}.

%%Update probData
%%allow_pass_update(ProbData, Position) ->
%%    {transfer, List} = lists:keyfind(transfer, 1, ProbData),
%%    NewTransferList = allow_pass_update(List,Position, []),
%%    lists:keyreplace(transfer,1, ProbData, {transfer, NewTransferList}).
%%allow_pass_update(-1, _Position, _NewTransferList) ->
%%    -1;
%%allow_pass_update([], _Position, NewTransferList) ->
%%    NewTransferList;
%%allow_pass_update([{LaneId, APass, Position} | Tail], Position, NewTransferList) ->
%%    allow_pass_update(Tail, Position, [{LaneId, APass - 1, Position} | NewTransferList]);
%%allow_pass_update([Transfer | Tail], Position, NewTransferList) ->
%%    allow_pass_update(Tail, Position, [Transfer| NewTransferList]).

%%=======================================================================%%
%%=======================================================================%%
%%=======================================================================%%    


%%=======================================================================%%
%%====================GENERAL FUNCTIONS==================================%%
%%=======================================================================%% 


%%TODO: Unir este metodo con attemp_transfer 
%% move cars on transfer success
%%keep_moving(CarsQueque, ObsPosition, LogData) ->
%%    io:format("keep moving CarsQueque ~w, ObsPosition ~w~n",[CarsQueque, ObsPosition]),
%%    write_result(LogData, io_lib:format("keep moving CarsQueque ~w, ObsPosition ~w~n",[CarsQueque, ObsPosition])),
%%    keep_moving(CarsQueque, ObsPosition, -1, [], LogData).

%%keep_moving([], _ObsPosition, _LastPosition, UpdatedCars, LogData)->
%%    io:format("STOP keep moving UpdatedCars ~w~n",[UpdatedCars]),
%%    write_result(LogData, io_lib:format("STOP keep moving UpdatedCars ~w~n",[UpdatedCars])),
%%    lists:reverse(UpdatedCars);

%%keep_moving([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ObsPosition,LastPosition, UpdatedCars, 
%%  LogData) when BPos - 1 /= ObsPosition, BPos - 1 /= LastPosition ->
%%    %%NewPosition = Position - 1,
%%    {NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, 1),
%%    io:format("keep moving OK NewPosition ~w. Tail ~w~n",[{NBPos, NEPos, Length}, Tail]),
%%    write_result(LogData, io_lib:format("keep moving OK NewPosition ~w. Tail ~w~n",[{NBPos, NEPos, Length}, Tail])),
%%    keep_moving(Tail, ObsPosition, NEPos, [{CarType,{Wait + 1,Delay, {NBPos, NEPos, Length}, Route, PrefLanes, NextMove, TopMove}} |  UpdatedCars],LogData);
    
%%keep_moving([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Tail], ObsPosition,LastPosition, UpdatedCars,
%%  LogData) when BPos - 1 == ObsPosition; BPos - 1 == LastPosition ->
%%    io:format("keep moving NO MORE~n",[]),
%%    write_result(LogData, io_lib:format("keep moving NO MORE~n",[])),
%%    keep_moving(Tail, ObsPosition, EPos,[{CarType,{Wait + 1,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} |  UpdatedCars], LogData).

%%Stop cars from moving after an obstacle has been found or when the red light has been given
stop_moving([{CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}}|Waiting]) ->
    io:format("STOPING CARS~n",[]),
    stop_moving(Waiting, Position, [{CarType,{Wait,Delay, Position, Route, PrefLanes, NextMove, TopMove}} | []]).
stop_moving(CarsQueque, LastPosition) ->
    io:format("STOPING CARS tow args~n",[]),
    stop_moving(CarsQueque, LastPosition, []).
stop_moving([], _LastPosition, UpdatedCars) -> 
    io:format("ALL CARS STOPED~n",[]),
    lists:reverse(UpdatedCars);
stop_moving([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Waiting], LastPosition, UpdatedCars) when LastPosition < BPos -1 ->
    io:format("CAR CAN MOVE~n",[]),
    {NBPos, NEPos, Length} = get_new_position({BPos, EPos, Length}, 1),
    stop_moving( Waiting, NEPos, [{CarType,{Wait + 1,Delay, {NBPos, NEPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars]);
stop_moving([{CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}|Waiting], LastPosition, UpdatedCars) when LastPosition >= BPos -1 ->
    NewUpdated = [{CarType,{Wait + 1,Delay + 1, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}} | UpdatedCars],
    io:format("CAR CAN NOt MOVE waiting: ~w ... Position ~w... NewUpdated: ~w ~n",[Waiting, {BPos, EPos, Length}, NewUpdated]),
    stop_moving( Waiting, EPos, NewUpdated).


%% Get the las car on the list
get_lastPosition([]) -> -1; %%{CarType,{0,0,-1,[],[],1,1}};
get_lastPosition(CarsQueque) -> 
    {_CarType,{_LastWait, _LastDelay, {_BPos, EPos, _Length}, _LastRout, _PrefLanes, _Next, _MaxMove}} = lists:last(CarsQueque),
    EPos.

%% Add cars to selected lanelane:
%% function used in estimate arrival if car entered goes to respective lane, 
%% if not enters the waiting list of the respective source lane
%% at any case, update times for cars lists
add_car({LaneId, LanePid, WaitingOutside, _ProbData, _Timer}, Car = {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, LogPath) ->    
    %%NewArrival  =  new_arrival({LaneId, LanePid,ProbData}),
    S = atom_to_list(LaneId),
    CPLaneId = list_to_atom(string:concat("out",string:concat(string:substr(S,8,2), string:substr(S,5,3)))),
    PreSendCar = {CarType,{Wait,Delay, {BPos, EPos, Length}, [{CPLaneId,Wait, Delay} | Route], PrefLanes, NextMove, TopMove}},
    io:format("Car to add ~w~n",[PreSendCar]),
    LanePid ! {incoming, self(), PreSendCar, Length},    
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
            WaitingUpdated;
            %%{[{LaneId, LanePid, WaitingUpdated, ProbData, NewArrival} | SourcesLane], NewArrival}
        {reply, {obs_on_begin, Any}} ->
            io:format("Lane ~w has obstruction at the begining. Adding to wait list~n",[LaneId]),
            write_result(LogPath, io_lib:format("Lane ~w has obstruction at the begining. Adding to wait list~n",[LaneId])), 
            NewWaitingOut = add_car_attemp(Any, PreSendCar, Car, WaitingOutside, LogPath),
            WaitingUpdated = waiting(NewWaitingOut, LogPath),
            %%WaitingUpdated = waiting([Car|WaitingOutside], LogPath),
            WaitingUpdated
        
    end.
    
    
get_new_position({BPos, _EPos, Length}, MovedSpaces) ->
    NewBPos = BPos - MovedSpaces,
    {NewBPos, NewBPos + Length, Length}.

available_space({CarType,{Wait, Delay, {_BPos, _EPos, Length}, Route, PrefLanes, NextMove, _TopMove}}, TopSpeed, LastPosition, LaneCap)
  when LastPosition == -1 ;  LastPosition + 1 + Length =<  LaneCap->
    NBPos = LaneCap - Length,
    NEPos = LaneCap,
    {CarType,{Wait, Delay, { NBPos, NEPos, Length}, Route, PrefLanes, NextMove, TopSpeed}};
available_space(_Car,_TopSpeed, _LastPosition, _LaneCap) ->
    false.

%%INPUT: List of altern siblings to the main lane so the car can enter the zone
%%	 Car to transfer
%%	 WaitingOutside: list of cars waiting outsite so far
%%OUTPUT: WaitingUpdated: list of cars with or without the Car to transfer according if the could enter the lane or not
%%DESC: This function is used to try to add a new car to the lane in case that there is an obstruction on the source lane
%%	it is required because if the car cannot longer enter the lane it will create a queque and the cars must enter
add_car_attemp([], _PreSendCar, Car, WaitingOutside, LogPath) ->
    io:format("The car could not be transfered to alt siblings, adding to waitlist~n",[]),
    write_result(LogPath, io_lib:format("The car could not be transfered to alt siblings, adding to waitlist~n",[])), 
    [Car | WaitingOutside];
add_car_attemp([{AltLaneId, AltLanePid} | SiblingsAlt], PreSendCar, Car, WaitingOutside, LogPath) ->
    io:format("Attemp to transfer to alt siblings ~w, Car ~w ~n",[{AltLaneId, AltLanePid}, PreSendCar]),
    write_result(LogPath, io_lib:format("Attemp to transfer to alt siblings ~w, Car ~w ~n",[{AltLaneId, AltLanePid}, PreSendCar])), 
    Res = add_car_attemp_aux({AltLaneId, AltLanePid}, PreSendCar),
    io:format("Attemp result ~w ~n",[Res]),
    write_result(LogPath, io_lib:format("Attemp result ~w ~n",[Res])), 
    case Res of
        {reply, ok}   -> io:format("Attemp SUCCEDED~n",[]),
			 write_result(LogPath, io_lib:format("Attemp SUCCEDED~n",[])),
        		 WaitingOutside;
        {reply, next} -> io:format("Attemp FAILED trying with next~n",[]),
			 write_result(LogPath, io_lib:format("Attemp FAILED trying with next~n",[])), 
        		 add_car_attemp(SiblingsAlt, PreSendCar, Car, WaitingOutside, LogPath)
    end.
add_car_attemp_aux({_LaneId, LanePid}, {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}) ->    
    LanePid ! {incoming, self(), {CarType,{Wait,Delay, {BPos, EPos, Length}, Route, PrefLanes, NextMove, TopMove}}, Length},    
    receive
    	{reply, ok}    ->
            {reply, ok};   
        _AnyOther  ->
            {reply, next}      
    end.


%%INPUT:  ConnectedLanes: list of al lanes connected to the current lane
%%	  ProbData: list of probs for the lane, needed to get the cedNum info
%%OUTPUT: List of CedNumPositions (if the lane has two o more siblings it will need a list of cedPositions
%%DESC:   Function to get the CedPosition for the current lane
locate_cedPositions(ConnectedLanes, ProbData, LogData) ->
    {siblings, SList} = lists:keyfind(siblings, 1, ConnectedLanes),
    {transfer, CedData} = lists:keyfind(transfer, 1, ProbData),
    locate_cedPositions_aux(SList, CedData, [], LogData).

locate_cedPositions_aux([], _CedData, CedPositions, _LogData) ->
    CedPositions;
locate_cedPositions_aux([{LaneId, LanePid} | Siblings], CedData, CedPositions, LogData) ->
    {_LaneId, CedNumCar} = lists:keyfind(LaneId, 1, CedData),
    case CedNumCar of
        0 -> LanePid ! {yield, self(), LogData},
             receive
                 {reply, CedPos} ->   locate_cedPositions_aux(Siblings, CedData, [{LaneId, CedPos} | CedPositions], LogData)             
             end;
        _Other ->
             locate_cedPositions_aux(Siblings, CedData, [{LaneId, -1} | CedPositions], LogData) 
           
    end.
    

%%INPUT:  Obstructions on the lane
%%	  CarsQueque: list of cars on the lane
%%OUTPUT: CedPosition or -1 if theres no need of it
%%DESC:   Function determine either the lane needs to used  the cedPosition or not.
gen_cedPositions([], _CarsQueque, _LogData) ->
    -1;
gen_cedPositions(_ObsData, [], _LogData) ->
    -1;
gen_cedPositions([ObsData | _Tail], CarsQueque, LogData) ->
    write_result(LogData, io_lib:format("Calling gen_cedPositions_aux ~w ",[{ObsData, CarsQueque}])), 
    gen_cedPositions_aux(ObsData, CarsQueque, LogData).

gen_cedPositions_aux(_ObsData, [], LogData) -> 
    write_result(LogData, io_lib:format("NO CedPos",[])),
    -1;    
gen_cedPositions_aux({_Obs, _ObsBPos, ObsEPos}, 
  [{CarType,{_Wait,_Delay, {BPos, EPos, _Length}, _Route, _PrefLanes, _NextMove, _TopMove}} | _Tail], LogData)
  when BPos - 1 == ObsEPos ->
    write_result(LogData, io_lib:format("CedPos for ~w on ~w~n",[CarType, EPos])), 
    EPos;
gen_cedPositions_aux({Obs, ObsBPos, ObsEPos}, [ _Car | Tail], LogData) ->
    gen_cedPositions({Obs, ObsBPos, ObsEPos}, Tail, LogData).
    



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
    Car = estimate_car_to_add(),
    io:format("ADDING CAR TO LANE FROM EMPTY OUTSIDE SOURCE ~n",[]),
    write_result(LogPath, io_lib:format("ADDING CAR ~w TO LANE FROM EMPTY OUTSIDE SOURCE. LANE ~w ~n",[Car, LaneId])),
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
    io:format("ADDING CAR ~w TO LANE FROM OCCUPIED OUTSIDE SOURCE ~n",[WaitingCar]),
    write_result(LogPath, io_lib:format("ADDING CAR ~w TO LANE FROM OCCUPIED OUTSIDE SOURCE. LANE ~w ~n",[WaitingCar,LaneId])),
    Car = estimate_car_to_add(),
    NewWaiting = lists:reverse([Car | lists:reverse(WaitingOutside)]),    
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
    io:format("ADDING CAR ~w TO LANE FROM OCCUPIED OUTSIDE SOURCE OFFSCHEDULE ~n",[WaitingCar]),
    write_result(LogPath, io_lib:format("ADDING CAR ~w TO LANE FROM OCCUPIED OUTSIDE SOURCE OFFSCHEDULE. LANE ~w ~n",[WaitingCar, LaneId])),
    %%NewWaiting = lists:reverse([{car,{0,0,-1,[],[]}} | lists:reverse(WaitingOutside)]),    
    %%{UpdatedSources, _NewArrival} = add_car({LaneId, LanePid, WaitingOutside, ProbData, Timer}, WaitingCar, SourcesLane, LogPath),
    WaitingUpdated = add_car({LaneId, LanePid, WaitingOutside,  ProbData, Timer}, WaitingCar, LogPath),
    estimate_new_arrival(Tail, [{LaneId, LanePid, WaitingUpdated,  ProbData, Timer - 1} | SourcesLane], LogPath).  

%%estimate_new_arrival([{LaneId, LanePid, WaitingOutside, ProbData, Timer}| Tail], SourcesLane, LogPath) ->
%%    io:format("NOT ADDING CAR TO LANE ~n",[]),
%%    write_result(LogPath, io_lib:format("NOT ADDING CAR TO LANE. LANE ~w ~n",[LaneId])),
%%    WaitingUpdated = waiting(WaitingOutside), 
%%    estimate_new_arrival(Tail, [{LaneId, LanePid, WaitingUpdated, ProbData, Timer - 1}|SourcesLane], LogPath).

estimate_car_to_add() ->
    estimate_car_to_add(random:uniform(2) - 1).     
estimate_car_to_add(0) -> %%a car just 1 space
    {car,{0,0,{-1,-1,0},[],[],1,1}};
estimate_car_to_add(1) -> %%a bus just 2 spaces->
    {bus,{0,0,{-1,-1,1},[],[],1,1}};
estimate_car_to_add(2) -> %%a trailer just 3 spaces
    {trailer,{0,0,{-1,-1,2},[],[],1,1}}.

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
    %io:format("Arrival time ~w~n",[Arrival]),
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
    -1;
allow_pass_siblings([], TransferList) ->
    TransferList;
allow_pass_siblings([LaneId | Tail], TransferList) -> 
	allow_pass_siblings(Tail, [{LaneId, 2} | TransferList]).
    %%allow_pass_siblings(Tail, [{LaneId, data_distribution(geoCedServer)} | TransferList]).

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

%%=======================================================================%%
%%====================CLIENT INTERFACE FUNC==============================%%
%%=======================================================================%%
%% remove if fails
connect(LanePid, ConnectionData) ->
	LanePid ! {connect_lane, ConnectionData, self()},
    receive
		{reply, error} -> 
			{reply, error};
	  	{reply, ok} -> 
		    {reply, ok};
		_Other ->
			{error, []}
    end.   

checkpoint({LaneId, LanePid}, LightData) ->
	LanePid ! {checkpoint, self(), LightData},
    receive
        {reply, chkpoint_saved} -> 
            io:format("reply recieve after checkpoint lane ~w.~n",[LaneId]),
            {reply, ok};
        _Other ->
        	{error, checkpoint}
    end.

restore(LaneList) ->
	{ok, Cwd} = file:get_cwd(),
    LanesFile = Cwd ++ "/lanes_chk.txt",
    CarFile = Cwd ++ "/Cars_chk.txt",
    OCarFile = Cwd ++ "/OCars_chk.txt",
    
	Lanes = filemanager:get_data(LanesFile),
	Cars = filemanager:get_data(CarFile),
	OCars = filemanger:get_data(OCarFile),
	
	restore_aux(LaneList, Lanes, Cars, OCars).


restore(LaneList, LanesFile, CarFile,OCarFile) ->
	io:format("Lanes Files: ~p", [{LanesFile, CarFile,OCarFile}]),
	Lanes = filemanager:get_data(LanesFile),
	Cars = filemanager:get_data(CarFile),
	OCars = filemanager:get_data(OCarFile),
	
	restore_aux(LaneList, Lanes, Cars, OCars).
   
%%=======================================================================%%
%%====================CLIENT INTERFACE FUNC==============================%%
%%=======================================================================%%
 
%% Write down the results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).
    
write_final_data([], Path) ->
    write_result(Path, io_lib:format("***End of car list",[]));
write_final_data([Car|Tail], Path) ->    
    write_result(Path, io_lib:format(" ~w",[Car])),
    write_final_data(Tail, Path).

write_final_stats([], _Path) ->
    [];
write_final_stats([{Stat, Counter}|Tail], Path) ->    
    write_result(Path, io_lib:format("~w: ~w",[Stat, Counter])),
    write_final_stats(Tail, Path).
    
write_checkpoint(LaneId, _LightId, _Dir, _Type, _ConnectedLanes, CarsQueque, OutSideArea, 
  Capacity, Obstruction, ProbData, Stats, TopSpeed, {Path, CarsPath,OCarsPath}) ->
	%%{lane212av1,light212,av, 1, [{main,[lane210av1]},{secondary, [lane210av2]}, {siblings, [lane212av2]}],[], source_lane, 5,
	%%Lanes = format_connected_lanes(ConnectedLanes),
	%%filemanager:write_raw(Path, 
	%%	io_lib:format("~w",[{LaneId,LightId, Dir, Type,Lanes, normal_lane,Capacity,Obstruction, ProbData, Stats, TopSpeed}])),
	filemanager:write_raw(Path, 
		io_lib:format("~w",[{LaneId, Capacity,Obstruction, ProbData, Stats, TopSpeed}])),
	filemanager:write_raw(CarsPath, io_lib:format("~w", [{LaneId, CarsQueque}])),
	filemanager:write_raw(OCarsPath, io_lib:format("~w", [{LaneId, OutSideArea}])).
   
format_connected_lanes(ConnectedLanes) ->
	lists:map(fun({LaneType, List}) ->
				{LaneType, lists:map(fun({LaneId, _LanePid}) -> LaneId end, List)} end,
			  ConnectedLanes).

%%%% RESTORE
%%restore_aux([], _Cars, _OCars, RestoredLanes) ->
%%	RestoredLanes;
%%restore_aux([Lanei | LaneTail], Cars, OCars, RestoredLanes) ->
%%	{LaneId,LightId, Dir, Type,Lanes, normal_lane,Capacity,Obstruction, ProbData, Stats, TopSpeed} = Lanei,
%%	CarsQueque = restore_cars(LaneId, Cars),
%%	OutSideArea = restore_cars(LaneId, OCars),
%%	RestLane= spawn(lane,init, 
%%		[restore, {LaneId, Type, Lanes, CarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed}]),
%%	restore_aux(LaneTail, Cars, OCars, [{LaneId, RestLane, LightId, Dir} | RestoredLanes]).

restore_aux([], _RestoredLanes, _Cars, _OCars) ->
	{ok, restore_aux};
restore_aux([Lanei | LaneTail], RestoredLanes, Cars, OCars) ->
	{LaneId,Pid, _LightController, _Dir, _ConnectedLanes, _IsSource, _ProbRanges}  = Lanei,
	LaneData = restore_lane(LaneId, RestoredLanes, Cars, OCars),
	if LaneData /= false ->
		Pid ! {restore, LaneData}; %% restore the data		
	   true -> nodata
	end,
	restore_aux(LaneTail, RestoredLanes, Cars, OCars).

restore_lane(LaneId, Lanes, Cars, OCars) ->
%%{LaneId, Capacity,Obstruction, ProbData, Stats, TopSpeed}
	LaneData = lists:keyfind(LaneId, 1, Lanes),
	case LaneData of
		false -> false;
		_Other ->
				{LaneId, Capacity,Obstruction, ProbData, Stats, TopSpeed} = LaneData,
				CarsQueque = restore_cars(LaneId, Cars),
				OutSideArea = restore_cars(LaneId, OCars),
				{CarsQueque, OutSideArea, Capacity, Obstruction, ProbData, Stats, TopSpeed}
	end.
	
restore_cars(Id, List) ->
	Res = lists:keyfind(Id, 1, List),
	case Res of
		false -> 	[];
		_Other -> 	{Id, Cars} = Res,
				 	Cars
	end. 
	
%%%SPECIAL METHOD FOR SOURCES LANES USED IN TRAFFIC MODULE
checkpoint_sources(Sources, CheckFile) ->
	lists:map(fun ({LaneId, _LanePid, WaitingCars, ProbData, Timer}) ->
				filemanager:write_raw(CheckFile, io_lib:format("~w", [{LaneId, {ProbData, Timer, WaitingCars}}]))
			  end,
			  Sources
			 ),
	{ok, checkpoint_sources}.


restore_sources(SourcesList, File) ->
	SourcesData = filemanager:get_data(File),
	restore_sources_aux(SourcesList, SourcesData, []).	
restore_sources_aux([], _SourcesData, RestoredSources) ->
	RestoredSources;
restore_sources_aux([{LaneId, LanePid, _WaitingCars, _ProbData, _Timer} | Tail], SourcesData, RestoredSources) ->
	{RestoredProbData, RestoredTimer, RestoredWaitingCars} = restore_cars(LaneId, SourcesData),
	restore_sources_aux(Tail, SourcesData, [ {LaneId, LanePid, RestoredWaitingCars, RestoredProbData, RestoredTimer} | RestoredSources]).


%%%%%%%%%%%%%%%%%%%%
%%SENSOR INTERFACE

%%update the counter by the amount specified
safe_sensor_update(Sensor, Dir, LaneId, Count) ->
	io:format("~nSAFE_SENSOR~n"),
	case Sensor of
		null   ->	{ok, nosensor};
		_Other ->	sensor:car_pass(Sensor, LaneId, Count, Dir)
	end.
	
