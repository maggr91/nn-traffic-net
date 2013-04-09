-module(lane).

-export([start/1, estimate_new_arrival/2]).

-export([init/1]).

%% Main call function to spawn proccess
start(Args) ->
    spawn(lane,init, [Args]).
    
init({Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData}) ->
    lane(Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData).

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

lane(Type, ConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData) ->
  %% For each lane on every street in the system, 
  %% get the move acording to the ligth state passed on messages
  %% and run the complete simulation
    receive
    %% if a go message is received it tells all cars to start to move
        {go, LightController, _TimeCycle, _Time, LogData} ->
            io:format("GO msj~n. CarsQueque: ~w~n",[{Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}]),
	    NewCarsQueque = move_cars(CarsQueque, ConnectedLanes,Obstruction, [], Capacity,ProbData),
	    io:format("Moving msj received from ~w. CarsQueque: ~w ~n",[LightController,NewCarsQueque]),
	    write_result(LogData, io_lib:format("Moving msj received from ~w",[LightController])),
	    reply(LightController, updated),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
    %% send a message to all cars to start to stop preventing them to pass the red ligth
        {stop, LightController, LogData} ->  	
	    NewCarsQueque = stop_moving(CarsQueque),
	    io:format("Stop msj received from ~w.~n",[LightController]),
	    write_result(LogData, io_lib:format("Stop msj received from ~w",[LightController])),
	    reply(LightController, updatedStop),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
	{waiting, LightController, _Time, LogData} ->
    %% When recieving a waiting message, update cars times if they can or cannot move
	    NewCarsQueque = waiting(CarsQueque),
	    io:format("Waiting msj received from ~w. LogData~p~n",[LightController,LogData]),
	    write_result(LogData, io_lib:format("Waiting msj received from ~w",[LightController])),
	    reply(LightController, updatedWaiting),
	    lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction, ProbData);
    %% connect_lane msg used to update ConnectedLanes list with any type of lane
	{connect_lane, AdjLane, Pid}  ->     		
    	    io:format("{Connected Lanes ~w.~n",[ConnectedLanes]),
    	    NewConnectedLanes = [AdjLane| ConnectedLanes],
     	    io:format("{new Connected Lanes ~w.~n",[NewConnectedLanes]),
    	    reply(Pid,ok),
    	    lane(Type, NewConnectedLanes, CarsQueque, Capacity, Obstruction, ProbData);
    %% incoming msg, used when a car arrives to lane
	{incoming, Pid, {car, {Wait, Delay, _Position}}} ->
	    io:format("Car incoming~n"),
    %% set car to the las position (capacity -1)
	    NewCarData = {car,{Wait,Delay,Capacity - 1}},
    %% get cars last position to evaluate if a new car can enter the lane
	    {car, {_LastWait, _LastDelay, LastPosition}} = get_lastPosition(CarsQueque),
    	    io:format("Car incoming ~w. LastCar Position: ~w~n",[NewCarData, LastPosition]),
    %% in case that lanes capactiy has not been reached and that the last position is free
    %% add the car to the end of the lane, then reply with an ok, if not reply with a full 
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

%% Get the las car on the list
get_lastPosition([]) -> {car, {0,0,-1}};
get_lastPosition(CarsQueque) -> lists:last(CarsQueque).


%%dispatch_cars([], _ConnectedLanes, _Time, _Pid) -> [];
%%dispatch_cars([_FirstCar|_Waiting], _ConnectedLanes, _Time, _Pid) -> true.


%% When a waiting message is recieved, cars have to move until they
%% reach the end of line
waiting([]) ->
    [];
waiting([{car,{Wait,Delay,Position}}|Tail]) -> 
    waiting([{car,{Wait,Delay,Position}}|Tail], [], Position).
waiting([], UpdatedCars, _LastPosition) -> 
    io:format("Updated waiting ~w, ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
%% update cars that are waiting outsite of sources lanes
waiting([{car,{Wait,Delay,Position}}|Tail], UpdatedCars, _LastPosition) when Position == -1-> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position}} | UpdatedCars], Position);  
%% in case that lass position has been reached or if theres a car ahead just update times and do not move the car
waiting([{car,{Wait,Delay,Position}}|Tail], UpdatedCars, LastPosition) when Position == 0; Position - 1 == LastPosition -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position}} | UpdatedCars], Position); 
%% when the next position is greather or equal to 0 and if there is no car update times and move forward
waiting([{car,{Wait,Delay,Position}}|Tail], UpdatedCars, LastPosition) when Position - 1 >= 0, Position - 1 /= LastPosition -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position - 1}} | UpdatedCars], Position - 1).


%% When a move message is recieved
move_cars([], _ConnectedLanes, _Obstruction, UpdatedCars, _LanCap, _ProbData) -> 
    io:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
%% if car is still on lane move it forward
move_cars([{car,{Wait,Delay, Position}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, ProbData) when Position - 1 >= 0 -> 
    move_cars(Tail, ConnectedLanes,[], [{car,{Wait + 1,Delay, Position - 1}} | UpdatedCars], LanCap, ProbData);
%% if car has reached the end of line, dispatch (send) car to one of connected lanes
move_cars([{car,{Wait,Delay, Position}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, ProbData) when Position - 1 < 0 -> 
    %%Dispatch Cars
    Prob = random:uniform(),
    {dispatch, ProbList} = lists:keyfind(dispatch, 1, ProbData),
    ProbRanges = list_to_tuple(ProbList),
    io:format("Prob dispatch: ~w.~n",[Prob]),
    %% try to dispatch car to connected lane and get result of it
    Res = prepare_car_dispatch({car,{Wait,Delay, Position}}, ConnectedLanes, Prob, Tail,ProbRanges),
    io:format("Dispatch result: ~w.~n",[Res]),
    %% if car was able to move to the next lane, continue with remaining cars, if not stop moving the rest of
    %% the cars
    case Res of
        {reply, transfered}        -> move_cars(Tail, ConnectedLanes, [], UpdatedCars, LanCap,ProbData);
        {reply, error, NewUpdated} -> stop_moving(NewUpdated, Position)
    end;
%% in case that thers and obstruction on the lane
move_cars(CarsQueque, ConnectedLanes, 
    		[ObsData | _Obstruction], _UpdatedCars, _LanCap, _ProbData)-> 
%% get probability for the current car to see if its enable to 
%% cross to a sibling lane
%% if it was not able, leave it on the same lane andu update times
%% and cotinue with the rest of the list
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
%% try to move the cars to the a sibling lane
    UpdatedCarsQueque = transfer_enabled(List, length(List), CarsQueque, ObsData),
    UpdatedCarsQueque.


%%Get the probability that the car goes either straight or turn in the corner
%%This calls car_dispatch and according to the prob gets the right lane to send the car
prepare_car_dispatch(Car, ConnectedLanes, Prob, CarsQueque,{Fprob, Mprob, _Eprob}) when Prob >= Fprob, Prob < Mprob->
    {Type, List} = lists:nth(1, ConnectedLanes),
    io:format("Prepare dispatch to lane: ~w.~n",[{Type, List}]),
    car_dispatch(Car, List, CarsQueque, self());
prepare_car_dispatch(Car, ConnectedLanes, Prob, CarsQueque, {_Fprob, Mprob, Eprob}) when Prob >= Mprob, Prob =< Eprob->
    {Type, List} = lists:nth(2, ConnectedLanes),
    io:format("Prepare dispatch to lane: ~w.~n",[{Type, List}]),
    car_dispatch(Car, List, CarsQueque, self()).


%% GET the car and try to
%% Send it to connected lane
car_dispatch(Car, [], CarsQueque, _CLanePid) ->
    io:format("Nolane. Adding to wait list~n",[]),
    {reply, error, [Car|CarsQueque]};
%% if the source lane and the target is the same the car has to leave the area.
car_dispatch(Car, [{_LaneId, LanePid} | _Tail], _CarsQueque, CLanePid) when LanePid == CLanePid ->
    io:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, Car]),
    {reply, transfered};
%% if its a differente lane, send an incoming msg to let it now a new car is comming
car_dispatch(Car, [{LaneId, LanePid} | _Tail], CarsQueque, CLanePid) when LanePid /= CLanePid ->
    io:format("Lane ~w to dispatch ~w. Car ~w ~n",[self(), {LaneId, LanePid}, Car]),
    LanePid ! {incoming, self(), Car},
    receive
        %% if a "full" reply is received then return original CarsQueque with the car
        {reply, full}  ->
            io:format("Lane ~w its full. Adding to wait list~n",[LaneId]),
	    {reply, error, [Car|CarsQueque]};
            %%CarsQuequeUpdated = waiting([Car|CarsQueque], []);
        %% if a "ok" reply is received then return transfered reply
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            {reply, transfered}
            %%CarsQuequeUpdated = waiting(CarsQueque,[]), 
            
    end.
    

%% In case that there are obstructions on the road, cars will try to change lanes (siblings)
%% and after that we get an updated list of remaining cars and move the others forward.
%% a lane has a max of 2 siblings so we use SiblingsNum var to determine which one to get
transfer_enabled(List, SiblingsNum, CarsQueque, {_Obs, _Begin, End}) when SiblingsNum == 1 ->
    [Sibling | _Tail] = List,
    attemp_transfer(Sibling, CarsQueque, [], End);
transfer_enabled(List, SiblingsNum, CarsQueque, {_Obs, _Begin, End}) when SiblingsNum /= 1 ->
    [Sibling | _Tail] = lists:reverse(List),
    attemp_transfer(Sibling, CarsQueque, [], End).


%% TODO:HACER que el carro trate de pasarse si puede cambia de linea si no, avanza siempre y cuando no haya obstrucion u otro carro
%% Attemp to transfer each car on the lane to sibling line    
attemp_transfer(_Sibling, [], UpdatedCars, _ObsPosition) ->
    lists:reverse(UpdatedCars);
%% if the car has reached the obstruction try to transfer, if "ok" move the rest of cars, if not test with other cars
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

%%TODO: Unir este metodo con attemp_transfer 
%% move cars on transfer success
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
      

%% Add cars to selected lane
%% function used in estimate arrival if car entered goes to respective lane, 
%% if not enters the waiting list of the respective source lane
%% at any case, update times for cars lists
add_car({LaneId, LanePid, WaitingOutside, ProbData}, Car, SourcesLane, LogPath) ->    
    LanePid ! {incoming, self(), Car},
    receive
        {reply, full}  ->
            io:format("Lane ~w its full. Adding to wait list~n",[LaneId]),
            write_result(LogPath, io_lib:format("Lane ~w its full. Adding to wait list",[LaneId])), 
            WaitingUpdated = waiting([Car|WaitingOutside]),
            [{LaneId, LanePid, WaitingUpdated, ProbData} | SourcesLane];
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            write_result(LogPath, io_lib:format("Card Added to lane ~w",[LaneId])),   
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
    write_result(LogPath, io_lib:format("ADDING CAR TO LANE FROM EMPTY OUTSIDE SOURCE. LANE ~w ~n",[LaneId])),
    Car = {car,{0,0,-1}},
    UpdatedSources = add_car({LaneId, LanePid, [], {Fprob, Mprob, Eprob}}, Car, SourcesLane, LogPath),
    estimate_new_arrival(Tail,random:uniform(), UpdatedSources, LogPath);
estimate_new_arrival([{LaneId, LanePid, [WaitingCar|WaitingOutside], {Fprob, Mprob, Eprob}}| Tail], Prob, SourcesLane, LogPath) when Prob >= Mprob, Prob =< Eprob  ->
    io:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE ~n",[]),
    write_result(LogPath, io_lib:format("ADDING CAR TO LANE FROM OCCUPIED OUTSIDE SOURCE. LANE ~w ~n",[LaneId])),
    NewWaiting = lists:reverse([{car,{0,0,-1}} | lists:reverse(WaitingOutside)]),
    UpdatedSources = add_car({LaneId, LanePid, NewWaiting, {Fprob, Mprob, Eprob}}, WaitingCar, SourcesLane, LogPath),
    estimate_new_arrival(Tail,random:uniform(), UpdatedSources, LogPath);
estimate_new_arrival([{LaneId, LanePid, WaitingOutside, {Fprob, Mprob, Eprob}}| Tail], Prob, SourcesLane, LogPath) when Prob >= Fprob, Prob < Mprob  ->
    io:format("NOT ADDING CAR TO LANE ~n",[]),
    write_result(LogPath, io_lib:format("NOT ADDING CAR TO LANE. LANE ~w ~n",[LaneId])),
    WaitingUpdated = waiting(WaitingOutside), 
    estimate_new_arrival(Tail,random:uniform(), [{LaneId, LanePid, WaitingUpdated, {Fprob, Mprob, Eprob}}|SourcesLane], LogPath).
    
%% Write down the results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).
