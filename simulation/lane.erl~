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
            io:format("GO msj~n. CarsQueque: ~w~n",[{Type, ConnectedLanes, CarsQueque, Obstruction, ProbData}]),
	    {NewCarsQueque, NewProbData, NewOutArea} = move_cars(CarsQueque, ConnectedLanes,Obstruction, [], Capacity,ProbData, LaneId, OutSideArea),
	    io:format("Moving msj received from ~w. CarsQueque: ~w ~n",[LightController,NewCarsQueque]),
	    write_result(LogData, io_lib:format("Moving msj received from ~w",[LightController])),
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
	    NewCarsQueque = waiting(CarsQueque),
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
	{incoming, Pid, {car, {Wait, Delay, _Position, Route}}} ->
	    io:format("Car incoming~n"),
    %% set car to the las position (capacity -1)
	    NewCarData = {car,{Wait,Delay,Capacity - 1, Route}},
    %% get cars last position to evaluate if a new car can enter the lane
	    {car, {_LastWait, _LastDelay, LastPosition, _LastRoute}} = get_lastPosition(CarsQueque),
    	    io:format("Car incoming ~w. LastCar Position: ~w~n",[NewCarData, LastPosition]),
    %% in case that lanes capactiy has not been reached and that the last position is free
    %% add the car to the end of the lane, then reply with an ok, if not reply with a full 
	    case ((length(CarsQueque) < Capacity) and (LastPosition < Capacity - 1))  of
	        true  ->	            
	            NewCarsQueque = lists:reverse([NewCarData|lists:reverse(CarsQueque)]),
	            reply(Pid, ok),
	            lane(LaneId, Type, ConnectedLanes, NewCarsQueque,OutSideArea, Capacity, Obstruction, ProbData);
	        false ->
	            reply(Pid, full),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction, ProbData)
	    end;
	{try_transfer, Pid, CedCarNum, {car,{Wait,Delay, Position, Route}}} ->
	    io:format("OutSchedule of cars incoming"),
	    %% Sacar probabilidad de que le den pasada al otro carril
            %% si no le dan pasada, dejarlo en el mismo carril si cambiar
            %% posiciones y aumentar contadores
            RemCap = length(CarsQueque) < Capacity,
	    case space_between_cars(CarsQueque, Position,{car,{Wait,Delay, Position, Route}}) of
	        {true, NewCarsQueque} when RemCap == true -> 
	            %%NewCarsQueque = transfer_at(Car,ConnectedLanes),
	            reply(Pid, ok),
	            lane(LaneId, Type, ConnectedLanes, NewCarsQueque,OutSideArea, Capacity, Obstruction, ProbData);
	        {false, _SameQueque} ->
	            reply(Pid, no_space),
	            lane(LaneId, Type, ConnectedLanes, CarsQueque,OutSideArea, Capacity, Obstruction,ProbData)
	    end;
	    %%case length(ExtraIncoming) < Capacity of
	    %%    true  ->	            
	    %%        NewCarsQueque = transfer_at(Car,ConnectedLanes),
	    %%        reply(Pid, ok),
	    %%        lane(Type, ConnectedLanes, NewCarsQueque, Capacity, Obstruction)
	    %%end;
	    
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
waiting([]) ->
    [];
waiting([{car,{Wait,Delay,Position, Route}}|Tail]) -> 
    waiting([{car,{Wait,Delay,Position, Route}}|Tail], [], Position).
waiting([], UpdatedCars, _LastPosition) -> 
    io:format("Updated waiting ~w, ~n",[UpdatedCars]),
    lists:reverse(UpdatedCars);
%% update cars that are waiting outsite of sources lanes
waiting([{car,{Wait,Delay,Position, Route}}|Tail], UpdatedCars, _LastPosition) when Position == -1-> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position, Route}} | UpdatedCars], Position);  
%% in case that lass position has been reached or if theres a car ahead just update times and do not move the car
waiting([{car,{Wait,Delay,Position, Route}}|Tail], UpdatedCars, LastPosition) when Position == 0; Position - 1 == LastPosition -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position, Route}} | UpdatedCars], Position); 
%% when the next position is greather or equal to 0 and if there is no car update times and move forward
waiting([{car,{Wait,Delay,Position, Route}}|Tail], UpdatedCars, LastPosition) when Position - 1 >= 0, Position - 1 /= LastPosition -> 
    waiting(Tail, [{car,{Wait + 1, Delay + 1, Position - 1, Route}} | UpdatedCars], Position - 1).
    
%%=================================================================================================================================
%%=================================================================================================================================

%%=======================================================================%%
%%=========================MOVING CARS FUNCTIONS=========================%%
%%=======================================================================%%

%% When a move message is recieved
move_cars([], _ConnectedLanes, _Obstruction, UpdatedCars, _LanCap, ProbData, _LaneId, NewOutArea) -> 
    io:format("No more cars to move: Moving carslist  ~w ~n",[UpdatedCars]),
    {lists:reverse(UpdatedCars), ProbData, NewOutArea};
%% if car is still on lane move it forward
move_cars([{car,{Wait,Delay, Position, Route}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea) when Position - 1 >= 0 -> 
    move_cars(Tail, ConnectedLanes,[], [{car,{Wait + 1,Delay, Position - 1, Route}} | UpdatedCars], LanCap, ProbData, LaneId, NewOutArea);
%% if car has reached the end of line, dispatch (send) car to one of connected lanes
move_cars([{car,{Wait,Delay, Position, Route}}|Tail], ConnectedLanes, [], UpdatedCars, LanCap, 
  ProbData, LaneId, NewOutArea) when Position - 1 < 0 -> 
    %%Dispatch Cars
    io:format("Prob dispatch: ~w.~n",[ProbData]),
    {dispatch, TurnCarNum} = lists:keyfind(dispatch, 1, ProbData),
    %%Get parent lane 
    S = atom_to_list(LaneId),
    ParentLaneId = list_to_atom(string:concat(string:substr(S,8,2), string:substr(S,5,3))),
    
    %% try to dispatch car to connected lane and get result of it
    {Res, NewTurnCarNum} = prepare_car_dispatch({car,{Wait,Delay, Position, Route}}, ConnectedLanes, Tail,TurnCarNum, ParentLaneId),
    NewProbData = lists:keyreplace(dispatch,1, ProbData, {dispatch, NewTurnCarNum}),
    io:format("Dispatch result: ~w.~n",[Res]),
    %% if car was able to move to the next lane, continue with remaining cars, if not stop moving the rest of
    %% the cars
    case Res of
        {reply, transfered, Car}   -> move_cars(Tail, ConnectedLanes, [], UpdatedCars, LanCap,NewProbData, LaneId, [Car|NewOutArea]);
        {reply, transfered}        -> move_cars(Tail, ConnectedLanes, [], UpdatedCars, LanCap,NewProbData, LaneId,NewOutArea);
        {reply, error, NewUpdated} -> io:format("CALL STOP MOVING ~w  position ~w.~n",[NewUpdated, Position]),
           			      {stop_moving(NewUpdated, Position), NewProbData, NewOutArea}
    end;
%% in case that thers and obstruction on the lane
move_cars(CarsQueque, ConnectedLanes, 
    		[ObsData | _Obstruction], _UpdatedCars, _LanCap, ProbData, _LaneId, _NewOutArea)-> 
%% get probability for the current car to see if its enable to 
%% cross to a sibling lane
%% if it was not able, leave it on the same lane and update times
%% and cotinue with the rest of the list
    {siblings, List} = lists:keyfind(siblings, 1, ConnectedLanes),
    {transfer, CedCarNum} = lists:keyfind(transfer, 1, ProbData),
%% try to move the cars to the a sibling lane
    UpdatedCarsQueque = transfer_enabled(List, length(List), CarsQueque, ObsData, CedCarNum),
    UpdatedCarsQueque.


%%Get the probability that the car goes either straight or turn in the corner
%%This calls car_dispatch and according to the prob gets the right lane to send the car
prepare_car_dispatch(Car, ConnectedLanes, CarsQueque,TurnCarNum, LaneId) when TurnCarNum == -1 ->
    {Type, List} = lists:keyfind(main, 1, ConnectedLanes),
    io:format("Prepare dispatch to straigth lane: ~w.~n",[{Type, List}]),
    {car_dispatch(Car, List, CarsQueque, self(), LaneId), TurnCarNum};
prepare_car_dispatch(Car, ConnectedLanes, CarsQueque,TurnCarNum, LaneId) when TurnCarNum > 0 ->
    {Type, List} = lists:keyfind(main, 1, ConnectedLanes),
    io:format("Prepare dispatch to straigth lane: ~w.~n",[{Type, List}]),
    Res = car_dispatch(Car, List, CarsQueque, self(),LaneId),
    io:format("return after prepare dispatch to straigth lane: ~w .  ~w ~n",[Res, TurnCarNum]),
    {Res, TurnCarNum - 1};
prepare_car_dispatch(Car, ConnectedLanes, CarsQueque, TurnCarNum, LaneId) when TurnCarNum == 0 ->
    {Type, List} = lists:keyfind(secondary, 1, ConnectedLanes),
    NewTurnCarNum = new_turn(),
    io:format("Prepare dispatch to alt lane: ~w.~n",[{Type, List}]),
    Res = car_dispatch(Car, List, CarsQueque, self(),LaneId),
    io:format("return after prepare dispatch to alt lane: ~w  ~w ~n",[Res, NewTurnCarNum]),
    %%{car_dispatch(Car, List, CarsQueque, self(),LaneId), NewTurnCarNum}.
    {Res, NewTurnCarNum}.


%% GET the car and try to
%% Send it to connected lane
car_dispatch(Car, [], CarsQueque, _CLanePid, _CPLaneId) ->
    io:format("Nolane. Adding to wait list~n",[]),
    {reply, error, [Car|CarsQueque]};
%% if the source lane and the target is the same the car has to leave the area.
car_dispatch({car,{Wait,Delay, Position, Route}}, [{_LaneId, LanePid} | _Tail], _CarsQueque, CLanePid, CPLaneId) when LanePid == CLanePid ->
    io:format("Same Lane ~w, dispatch outside area. Car ~w ~n",[{LanePid, CLanePid}, {car,{Wait,Delay, Position, [CPLaneId|Route]}}]),
    {reply, transfered, {car,{Wait,Delay, Position, [CPLaneId|Route]}}};
%% if its a differente lane, send an incoming msg to let it now a new car is comming
car_dispatch(Car = {car,{Wait,Delay, Position, Route}}, [{LaneId, LanePid} | _Tail], CarsQueque, CLanePid,CPLaneId) when LanePid /= CLanePid ->
    PreSendCar = {car,{Wait,Delay, Position, [CPLaneId | Route]}},
    io:format("Lane ~w to dispatch ~w. Car ~w ~n",[CPLaneId, LaneId, Car]),
    LanePid ! {incoming, self(), PreSendCar},
    receive
        %% if a "full" reply is received then return original CarsQueque with the car
        {reply, full}  ->
            io:format("Lane ~w its full on car dispatch. Adding to wait list~n",[LaneId]),
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
transfer_enabled(List, SiblingsNum, CarsQueque, {_Obs, _Begin, End}, CedCarNum) when SiblingsNum == 1 ->
    [Sibling | _Tail] = List,
    attemp_transfer(Sibling, CarsQueque, [], End, CedCarNum);
transfer_enabled(List, SiblingsNum, CarsQueque, {_Obs, _Begin, End}, CedCarNum) when SiblingsNum > 1 ->
    TransferSibling = random:uniform(SiblingsNum),
    Sibling = lists:nth(TransferSibling, List),
    attemp_transfer(Sibling, CarsQueque, [], End, CedCarNum).


%% TODO:HACER que el carro trate de pasarse si puede cambia de linea si no, avanza siempre y cuando no haya obstrucion u otro carro
%% Attemp to transfer each car on the lane to sibling line    
attemp_transfer(_Sibling, [], UpdatedCars, _ObsPosition, _CedCarNum) ->
    lists:reverse(UpdatedCars);
%% if the car has reached the obstruction try to transfer, if "ok" move the rest of cars, if not test with other cars
attemp_transfer(Sibling, [{car,{Wait,Delay, Position, Route}} | Tail], UpdatedCars, ObsPosition, 
  CedCarNum) when Position -1 == ObsPosition->
    %% When they reached the obstacule stop the cars on the lane and just update times
    Sibling ! {try_transfer, self(), CedCarNum, {car,{Wait,Delay, Position, Route}}},
    receive
        {ok, NewCedCarNum}     -> move_on_transfer_succ(Tail,Position);
        {error, NewCedCarNum}  -> stop_moving(Tail, Position, [ {car,{Wait + 1,Delay + 1, Position, Route}} | UpdatedCars])
    end;

attemp_transfer(Sibling, [{car,{Wait,Delay, Position, Route}} | Tail], UpdatedCars, ObsPosition, 
  CedCarNum) when Position -1 /= ObsPosition ->
    %% When they have not reached the obstacule 
    attemp_transfer(Sibling, Tail, [{car,{Wait + 1,Delay, Position - 1, Route}} | UpdatedCars], ObsPosition, CedCarNum).

%%TODO: THIS SHOULD BE USED FOR TRANSFER AT ANY PLACE OF THE LANE
%%attemp_transfer(Sibling, [{car,{Wait,Delay, Position, Route}} | Tail], UpdatedCars, ObsPosition) when Position -1 /= ObsPosition ->
%%    %% when they haven't reached the obstacule try to pass the car to the sibling lane if possible pass if not continue moving
%%    %% until it reaches the obstacle
%%    Sibling ! {try_transfer, self(), {car,{Wait,Delay, Position, Route}}},
%%    receive
%%        ok     -> attemp_transfer(Sibling, Tail, UpdatedCars, ObsPosition);
%%       error  -> attemp_transfer(Sibling, [{car,{Wait + 1,Delay, Position - 1, Route}} | Tail], UpdatedCars, ObsPosition)
%%    end.

%%TODO: Unir este metodo con attemp_transfer 
%% move cars on transfer success
move_on_transfer_succ(CarsQueque, LastPosition) ->
    move_on_transfer_succ(CarsQueque, LastPosition, []).
move_on_transfer_succ([], _LastPosition, UpdatedCars) ->
    lists:reverse(UpdatedCars);
move_on_transfer_succ([{car,{Wait,Delay, Position, Route}} | Tail], LastPosition, UpdatedCars) when Position - 1 >= LastPosition ->
    move_on_transfer_succ(Tail, Position, [{car,{Wait,Delay, Position - 1, Route}} | UpdatedCars]).

%%Stop cars from moving after an obstacle has been found or when the red light has been given
stop_moving([{car,{Wait,Delay, Position, Route}}|Waiting]) ->
    io:format("STOPING CARS~n",[]),
    stop_moving(Waiting, Position, [{car,{Wait,Delay, Position, Route}} | []]).
stop_moving(CarsQueque, LastPosition) ->
    io:format("STOPING CARS tow args~n",[]),
    stop_moving(CarsQueque, LastPosition, []).
stop_moving([], _LastPosition, UpdatedCars) -> 
    io:format("ALL CARS STOPED~n",[]),
    lists:reverse(UpdatedCars);
stop_moving([{car,{Wait,Delay, Position, Route}}|Waiting], LastPosition, UpdatedCars) when LastPosition < Position -1 ->
    io:format("CAR CAN MOVE~n",[]),
    stop_moving( Waiting, Position - 1, [{car,{Wait + 1,Delay, Position - 1, Route}} | UpdatedCars]);
stop_moving([{car,{Wait,Delay, Position, Route}}|Waiting], LastPosition, UpdatedCars) when LastPosition >= Position -1 ->
    NewUpdated = [{car,{Wait + 1,Delay + 1, Position, Route}} | UpdatedCars],
    io:format("CAR CAN NOt MOVE waiting: ~w ... Position ~w... NewUpdated: ~w ~n",[Waiting, Position, NewUpdated]),
    stop_moving( Waiting, Position, NewUpdated).




    
%% Transfer car at indicated Position of the Queque
%transfer_at([], Car, _CarCount) ->
%    [Car|[]];
%transfer_at([{car,{Wait,Delay, Position}} | CarsTail], Car, CarCount) when CarCount == 1 ->
%    transfer_at({car,{Wait,Delay, Position}}, CarsTail, Car);
%transfer_at([{car,{Wait,Delay, Position}} | CarsTail], Car, CarCount) when CarCount >= 2 ->
%    transfer_at({car,{Wait,Delay, Position}}, CarsTail, Car).

%% Get the las car on the list
get_lastPosition([]) -> {car, {0,0,-1,[]}};
get_lastPosition(CarsQueque) -> lists:last(CarsQueque).

%%check space between cars
space_between_cars([], _TransferPosition, CaToTransfer) ->
    {true, [CaToTransfer|[]]};
space_between_cars(CarsList, TransferPosition, CaToTransfer) when length(CarsList) > 1 ->
   [Car | Tail] = CarsList,
   is_space_enabled(Car, Tail, TransferPosition, CaToTransfer, []);    
space_between_cars(CarsList, TransferPosition, CaToTransfer) when length(CarsList) == 1 ->
   [Car | _Tail] = CarsList,
   is_space_enabled(Car, TransferPosition, CaToTransfer, CarsList).


%% check for space aviability at position X and X-1
%% try to insert car when there is just one car on the line
is_space_enabled({car,{Wait, Delay, Position, Route}}, TransferPosition, CaToTransfer, _CarsQueque) when Position < TransferPosition ->
    NewQueque = [CaToTransfer|[]],
    {true, [{car,{Wait, Delay, Position, Route}}|NewQueque]};
is_space_enabled({car,{_Wait, _Delay, Position, _Route}}, TransferPosition, CaToTransfer, CarsQueque) when Position > TransferPosition, TransferPosition + 1 /=  Position ->
    {true, [CaToTransfer|CarsQueque]};
is_space_enabled({car,{_Wait, _Delay, Position, _Route}}, TransferPosition, _CaToTransfer, CarsQueque) when Position > TransferPosition, TransferPosition + 1 ==  Position ->
    {false,CarsQueque}.

%% try to insert car when there are two o more cars on the line
is_space_enabled([], _NoTail, _TransferPosition, _CaToTransfer, UpdatedCars) ->
    {false, lists:reverse(UpdatedCars)};
is_space_enabled(FirstCar = {car,{_WaitL, _DelayL, PositionL, _RouteL}}, CarsQueque = [{car,{Wait,Delay, Position, Route}} | CarsTail], TransferPosition, CaToTransfer, UpdatedCars) 
  when TransferPosition > PositionL  ->
    case in_range(PositionL, Position, TransferPosition) of
        {true, Diff} when Diff >= 2 -> 
           NewUpdated = [FirstCar | UpdatedCars],
           AddCarList = [CaToTransfer | NewUpdated],
           lists:append(lists:reverse(AddCarList), CarsQueque);
        {true, Diff} when Diff < 2 ->          
           NewUpdated = [FirstCar | UpdatedCars],
           lists:append(lists:reverse(NewUpdated), CarsQueque); 
        {false, no_in_range} ->
           is_space_enabled({car,{Wait,Delay, Position, Route}}, CarsTail, TransferPosition, CaToTransfer, [FirstCar|UpdatedCars]);
        _ -> 
           NewUpdated = [FirstCar | UpdatedCars],
           lists:append(lists:reverse(NewUpdated), CarsQueque)
            %%no space to transfer 
           
    end;
    
is_space_enabled({car,{WaitL,DelayL, PositionL, RouteL}}, CarsTail, TransferPosition, CaToTransfer, UpdatedCars) 
  when TransferPosition < PositionL  ->
    case in_range(PositionL, TransferPosition) of
        {true, Diff} when Diff >= 2 ->
           NewUpdated = [CaToTransfer | UpdatedCars],
           lists:append(lists:reverse([{car,{WaitL,DelayL, PositionL, RouteL}} | NewUpdated]), CarsTail);
        _ -> 
           [{car,{WaitL,DelayL, PositionL, RouteL}}| CarsTail]
    end.      
    
%% Check if TransferPosition(Match) is located between the current position of first car (Min)
%% and the next one (Max).
in_range(Min, Max, Match) when Match > Min, Match < Max->
    {true, Max - Match};
in_range(Min, Max, Match) when Match > Min, Match > Max->
    {false, no_in_range};
in_range(_Min, _Max, _Match) -> 
    {false, -1}. 
in_range(Min, Match) when Match < Min->
    {true, Min - Match};
in_range(_Min,_Match) -> 
    {false, -1}.

%%Update probData
allow_pass_update(ProbData, Position) ->
    {transfer, List} = lists:keyfind(transfer, 1, ProbData),
    NewTransferList = allow_pass_update(List,Position, []),
    lists:keyreplace(transfer,1, ProbData, {transfer, NewTransferList}).
allow_pass_update(-1, Position, _NewTransferList) ->
    -1;
allow_pass_update([], Position, NewTransferList) ->
    NewTransferList;
allow_pass_update([{LaneId, APass, Position} | Tail], Position, NewTransferList) ->
    allow_pass_update(Tail, Position, [{LaneId, APass - 1, Position} | NewTransferList]);
allow_pass_update([Transfer | Tail], Position, NewTransferList) ->
    allow_pass_update(Tail, Position, [Transfer| NewTransferList]).


%% Add cars to selected lanelane:
%% function used in estimate arrival if car entered goes to respective lane, 
%% if not enters the waiting list of the respective source lane
%% at any case, update times for cars lists
add_car({LaneId, LanePid, WaitingOutside, _ProbData, _Timer}, Car, LogPath) ->    
    %%NewArrival  =  new_arrival({LaneId, LanePid,ProbData}),
    LanePid ! {incoming, self(), Car},    
    receive
        {reply, full}  ->
            io:format("Lane ~w its full. Adding to wait list~n",[LaneId]),
            write_result(LogPath, io_lib:format("Lane ~w its full. Adding to wait list",[LaneId])), 
            WaitingUpdated = waiting([Car|WaitingOutside]),
            WaitingUpdated;
            %%{[{LaneId, LanePid, WaitingUpdated, ProbData, NewArrival} | SourcesLane], NewArrival};
        {reply, ok}    ->
            io:format("Card Added to lane ~w~n",[LaneId]),
            write_result(LogPath, io_lib:format("Card Added to lane ~w",[LaneId])),   
            WaitingUpdated = waiting(WaitingOutside),
            WaitingUpdated
            %%{[{LaneId, LanePid, WaitingUpdated, ProbData, NewArrival} | SourcesLane], NewArrival}
    end.

  

%%add_car({LaneId, LanePid, WaitingOutside, ProbData, ArrivalTimer}, _Car, SourcesLane, _LogPath, NewArrival) ->
%%    Car = {car,{0,0,-1}},
%%    Arrival  =  new_arrival({LaneId, LanePid,ProbData}),
%%    [ {LaneId, LanePid, WaitingOutside, ProbData, Arrival} | SourcesLane ];

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
    Car = {car,{0,0,-1,[]}},
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
    NewWaiting = lists:reverse([{car,{0,0,-1,[]}} | lists:reverse(WaitingOutside)]),    
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
    %%NewWaiting = lists:reverse([{car,{0,0,-1,[]}} | lists:reverse(WaitingOutside)]),    
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
    io:format("Turning Car number ~w~n",[Turn]),
    NewLane = {LaneId, LC,Dir, Type, ConnectedLanes, 
                Cars, IsSource, Capacity, ProbData, [Turn,AllowPass]},
    init_geometric(Tail, [NewLane | LanesUpdated]);
init_geometric([{LaneId, LC,Dir, Type, ConnectedLanes, 
                Cars, IsSource, Capacity, ProbData}| Tail], LanesUpdated) when Type == 1 ->
    Turn = {dispatch, -1},
    AllowPass = {transfer, data_distribution(geoCedServer)},
    io:format("Turning Car number ~w~n",[Turn]),
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
    
