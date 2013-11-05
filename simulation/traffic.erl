-module(traffic).

-export([start/2, set_map/1, connect/2, connect_lanes/1]).

-export([init/2]).

%-export([test_move_av/1,test_move_ca/1, test_idle/1]).

%-import(light_fsm,[init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%client calls
-import(light_fsm,[start_link/0,move_avenue/1, move_street/1, idle/1,update_siblings/2, evaluate_state/1]).


%% This module is used to run the simulation
%% of normal traffic, with no modification of ANN

%% Start method, USE this to run simulation
start(Mode, ConfigData) ->    
	spawn(traffic, init, [Mode, ConfigData]).
	
%%restore( LanesFile, CarsFile, OCarFile, ConfigData, Time) ->
%%	spawn(traffic, init, [checkpoint, {LanesFile, CarsFile, OCarFile, ConfigData, Time}]).
		
reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

init(restore, {ConfigData, Time}) ->
	%%loop({[], [], {"LogPath", "DataLogPath", "CheckPointLog"}}, Time);
	LightSource = find_config_data(ConfigData, source_lights),
	LaneSource = find_config_data(ConfigData, source_lanes),
	ArrivaLog = find_config_data(ConfigData, log_arrival),
	DataLog = find_config_data(ConfigData, log_data),
    CheckPointLog = find_config_data(ConfigData, checkpoint_data),
    AvgCar = find_config_data(ConfigData, avg_car_length),
    
    MapData = restore_map({LightSource, LaneSource, ArrivaLog, DataLog, CheckPointLog, AvgCar}), 
	loop(MapData, Time);

init(normal, {MaxSpeed, ConfigData}) ->
	%% get all config info
	io:format("Configuration ~p.~n",[ConfigData]), 
	LightSource = find_config_data(ConfigData, source_lights),
	LaneSource = find_config_data(ConfigData, source_lanes),
	ObsSource = find_config_data(ConfigData, source_obs),
	AvgCar = find_config_data(ConfigData, avg_car_length),
	ArrivaLog = find_config_data(ConfigData, log_arrival),
	DataLog = find_config_data(ConfigData, log_data),
    CheckPointLog = find_config_data(ConfigData, checkpoint_data),
    
    io:format("Config Data ~w.~n",[{MaxSpeed, LightSource, LaneSource, ObsSource, AvgCar, ArrivaLog,DataLog}]),
    MapData = set_map({MaxSpeed, LightSource, LaneSource, ObsSource, AvgCar, ArrivaLog,DataLog, CheckPointLog}), 
    loop(MapData, 0).

%%find configuration
find_config_data(ConfigData, Key) ->
	Res = lists:keyfind(Key, 1, ConfigData),
	case Res of
		false -> [];
		{Key, Value} -> Value
	end.

%% Function to load the map by reading files located in
%% ~/sources folder
set_map({MaxSpeed, LightSource, LaneSource, ObsSource, AvgCar, ArrivaLog,DataLog, CheckPointLog}) ->
%%Read data from files
    Axis = get_lights(LightSource),
    Roads = get_lanes(LaneSource),
    Obs = get_obs(ObsSource),
    
    %%get the max move for each car on the area
    MaxSpeedMove = max_car_move(MaxSpeed, AvgCar),

    io:format("Allocating lights ~w.~n",[Axis]),
    {_Origin, AllocatedLights} = allocate_lights({Axis,[]}),
%%Get geometric distribution for each lane that has a type 2 (2 directions)
    NewRoads = lane:init_geometric(Roads),    
%%Spawn every lane as a proccess    
    {_OrigLanes, AllocatedLanes} = allocate_lanes({NewRoads,[],Obs,MaxSpeedMove}),
%%Set archive_log for cars arrival to sources_lanes  
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/logs/",
    io:format("PATH ~s.~n",[Path]),    

    NewPath = Path ++ ArrivaLog,
    DataPath = Path ++ DataLog,
    NewCheckLogPath = format_checkpointlogs(CheckPointLog),
    
    file:delete(NewPath), %% delete old arrival log
    file:delete(DataPath), %% delete old data log
    
%%Connect each lane with siblings or any other connected lane
%%after that get sources lanes (lanes where cars will arrive to the area)    
    connect_lanes({AllocatedLanes,AllocatedLanes}),
    SourceLanes = get_source_lanes(AllocatedLanes),

%%Get poisson dist for each source lane
    NewSources = lane:init_poisson(SourceLanes),    
    %%io:format("SOURCES With poisson ~w.~n",[NewSources]),    
    io:format("PREPARING LightsFSM .~n",[]),
%%Spawn FSM for traffic lights on each intersection and then connect them    
    LightsFSM = connect({AllocatedLights, AllocatedLanes, [],Path}, normal), 
    io:format("LightsFSM ~w.~n",[LightsFSM]),
    set_connection_to_light_siblings(LightsFSM,LightsFSM),

	FinalLightsFSM = light_final_format(LightsFSM),
	
    {FinalLightsFSM, NewSources, {NewPath,DataPath, NewCheckLogPath}}.


%% DEPRECATED: Spawn every intersection and lane on the map
%% modify to avoid this step
allocate_lights({[],Spawned})->
    {[],Spawned};
allocate_lights({[{LightId, Sequence, Siblings, Times}|Tail], Spawned}) ->
    %%Pid = spawn(traffic,light, [ManagedLanes,Siblings, Cycle_time, Go_time]),
    %%Replace Go_time for  AllRed_time (Go_time set by default to 0
    %%allocate_lights({Tail, [{LightId,Siblings, Cycle_time,Go_time, AllRed_time} | Spawned]}). 
    allocate_lights({Tail, [{LightId,Sequence, Siblings, Times} | Spawned]}). 
 
 
%% Spawn every lane on the area with the respective data         
allocate_lanes({[], Spawned,_Obs,_MaxSpeedMove}) ->
    {[],Spawned};
allocate_lanes({[{LaneId,LightController,Dir, Type, ConnectedLanes, 
                  CarsQueque, IsSource, Capacity, {probList, ProbData}, GeoProb}|Tail], Spawned, Obs, MaxSpeedMove}) ->
%%for each lane in the list spawn a proccess
    %%Pid = lane:start({Type, [], CarsQueque, Capacity, [], ProbData, GeoProb}),
    %%Obstruction = [create_obstruction(Capacity)],
    %io:format("Obstacules loaded: ~w",[Obs]),
    Obstruction = allocate_obs(Obs, LaneId),
    
    TargetMaxSpeed = find_max_car_move(LaneId, Dir, MaxSpeedMove),
      
    Pid = lane:start(normal,{LaneId, Type, [], CarsQueque, Capacity, Obstruction, GeoProb, TargetMaxSpeed}),
    {arrival, ProbList} = lists:keyfind(arrival, 1, ProbData),
    ProbRanges = list_to_tuple(ProbList),
    allocate_lanes({Tail, [{LaneId,Pid, LightController,Dir,ConnectedLanes, IsSource, ProbRanges} | Spawned], Obs, MaxSpeedMove}).

%%connect lights with its lanes
connect({[], _LaneList, LightsFSM,_LogData}, _Mode) -> 
    LightsFSM;
connect({[{LightId,Sequence,Siblings, Times} | TailLight], LaneList,LightsFSM, LogData}, Mode) -> 
    %%io:format("Luz: ~w / lineas: ~w.~n",[LightId, LaneList]),
    %%{ManagedLanes, RemLanes} = find_lanes({LightId, {[],[]}}, LaneList, []),
    {ManagedLanes, RemLanes} = find_lanesO({LightId, []}, LaneList, []),
    %%io:format("Managed Lanes: ~w.~n~n",[ManagedLanes]),
    PathLog = (LogData ++ lists:flatten(io_lib:format("~p",[LightId]))) ++ ".txt",
    {ok, LightPid} = invoke_light(Sequence, start_link, {Mode, LightId, ManagedLanes,Siblings, Times, PathLog}),
    connect({TailLight, RemLanes, [{LightId,LightPid, Sequence}|LightsFSM], LogData}, Mode).

%%invoke_light(twoDir, Args) ->
%%	light_fsm:start_link(Args);
%%invoke_light(threeDir, Args) ->
%%	light_fsm_3st:start_link(Args).

invoke_light(twoDir,Func, Args) ->
	light_fsm:Func(Args);
invoke_light(threeDir,Func, Args) ->
	light_fsm_3st:Func(Args).
	
%%%%%%%%%%%%%%%%%%%%%%%%%NEW METHOD TO ADD LANES
find_lanesO({_LightId,ManagedLanes}, [], RemLanes) ->     
    {lists:reverse(ManagedLanes), RemLanes};
find_lanesO({LightId,ManagedLanes}, [{LaneId,LanePid, LightId,Dir,_AdjLanes, _IsSource, _ProbData}| Tail], RemLanes) ->
	%% Get the lane type list for the current lane and add it
    %%Determine lane type if its either a street or an avenue and locate it in the corresponding list
    %%io:format("New line ~w~n",[Dir]),
    io:format("Light ~w ManagedLanes ~w ~n",[LightId, ManagedLanes]),
    Res = lists:keyfind(Dir, 1, ManagedLanes),
    case Res of
    	{Dir, List} ->	NewList = [{LaneId,LanePid} | List],
					    NewManaged = lists:keyreplace(Dir,1, ManagedLanes, {Dir, NewList});
		false 		->	NewManaged = [{Dir, [{LaneId,LanePid}]} | ManagedLanes]		
    end,
    find_lanesO({LightId,NewManaged},Tail, RemLanes);
        
find_lanesO({LightId, ManagedLanes}, [LHead | LTail], RemLanes) ->
    find_lanesO({LightId, ManagedLanes}, LTail, [LHead|RemLanes]).



%% Connect each lane with other near lanes
%% get near lanes list and iterate the list
connect_lanes({[], _LaneList}) -> 
    {[],{ok, ready}};
connect_lanes({[{LaneId,Pid, _Light,_Dir, AdjLanes, _IsSource, _ProbData} | TailLane], LaneList}) -> 
    %%io:format("Carril: ~w / lineas adj: ~w.~n",[{LaneId,Pid}, AdjLanes]),
    set_connection_to_lane({LaneId,Pid}, AdjLanes, LaneList),
    connect_lanes({TailLane, LaneList}).
  
%% For each type of adjLane (Main, secondary, or sibling) get the corresponding list
%% and look for the proccess for the especified ID.
set_connection_to_lane({_LaneId, _LanePid}, [], _LanesList) -> 
    [];
set_connection_to_lane({LaneId,LanePid}, [{Type, AdjLaneTypeList} | AdjLanesTypeTail], LaneList) ->    
    AdjLaneTypeData = iterate_Adj_lane_types(AdjLaneTypeList,LaneList,[]),    
    Res = lane:connect(LanePid, {Type, AdjLaneTypeData}),    
    case Res of
        {reply, error} -> 
        	error;
  		{reply, ok} ->         
            set_connection_to_lane({LaneId,LanePid},AdjLanesTypeTail, LaneList)
    end.   

%%if fail comment above and uncomment this
%%set_connection_to_lane({_LaneId, _LanePid}, [], _LanesList) -> 
%%    [];
%%set_connection_to_lane({LaneId,LanePid}, [{Type, AdjLaneTypeList} | AdjLanesTypeTail], LaneList) ->    
%%    AdjLaneTypeData = iterate_Adj_lane_types(AdjLaneTypeList,LaneList,[]),    
%%    LanePid ! {connect_lane, {Type, AdjLaneTypeData}, self()},    
%%    receive
%%        {reply, error} -> 
%%        	error;
%%  		{reply, ok} ->         
%%            set_connection_to_lane({LaneId,LanePid},AdjLanesTypeTail, LaneList)
%%    end.   




%% For the corresponding lane type, iterate the lane list and find in the corresponding PID
iterate_Adj_lane_types([],_LaneList, TypeAdjList) -> lists:reverse(TypeAdjList);
iterate_Adj_lane_types([AdjLane | AdjLanesTail],LaneList, TypeAdjList) ->
    AdjLaneData = find_adjLanes(AdjLane,LaneList),
    iterate_Adj_lane_types(AdjLanesTail, LaneList, [AdjLaneData | TypeAdjList]).    
    
%% Find all corresponding lane PID, for the indicated AdjLaneId  
find_adjLanes(_AdjLaneId, []) -> 
    %%io:format("No Coincidencia: ~w continue.~n",[AdjLaneId]),
    {};
find_adjLanes(AdjLaneId, [{AdjLaneId,AdjLanePid, _Light,_Dir, _AdjLanes, _IsSource, _ProbData}|_Tail]) ->
    {AdjLaneId,AdjLanePid};  
find_adjLanes(AdjLaneId, [_LHead | LTail]) ->
    find_adjLanes(AdjLaneId, LTail).


%% Use the List FSM lights to get the list of siblings that each one has.
set_connection_to_light_siblings([], _LightsList) -> [];
set_connection_to_light_siblings([{_Id, Pid,Sequence}|LightsRem], LightsList) ->
    %%{_State, _Times, Siblings, _LogData, _OldState} = light_fsm:get_state(Pid),
   {_State, _Times, Siblings, _LogData, _OldState, _CtrlMod} = invoke_light(Sequence, get_state, Pid),
%% The siblings list is a list of tuples that contaings LightId use this to find the corresponding PID
    CompleteSiblings = find_siblings(Siblings,LightsList,[]),
%% After getting the list with all PIDs, update the corresponding FSM    
    %%light_fsm:update_siblings(Pid, CompleteSiblings),
    invoke_light(Sequence, update_siblings, {Pid, CompleteSiblings}),
    set_connection_to_light_siblings(LightsRem,LightsList).

%% used to iterate throught all directions where the light has a sibling (av, ca, etc)
find_siblings([], _LightsList, CompleteSiblings) ->
	CompleteSiblings;
find_siblings([{Dir, DirLightList} | TailSiblings], LightsList, CompleteSiblings) ->
	FixedSiblings = find_siblings_aux(DirLightList, LightsList, []),
	find_siblings(TailSiblings, LightsList, [{Dir, FixedSiblings} | CompleteSiblings]).
    
%% Iterate sibling's list and find the corresponding data in the lights_FSM list
%% for this we use the lists:keyfind/3 to return a tuple {LightId, LightPid}
find_siblings_aux([],_LightsList, CompleteSiblings) ->
    lists:reverse(CompleteSiblings);
%%find_siblings_aux([SiblingId|Tail], LightsList,CompleteSiblings) ->
find_siblings_aux([{SiblingId, Location}|Tail], LightsList,CompleteSiblings) ->
    SiblingData = lists:keyfind(SiblingId, 1, LightsList),
    case SiblingData of
		false ->  find_siblings_aux(Tail,LightsList, CompleteSiblings);
				
		_Other -> {Id, Pid,Sequence} = SiblingData,
    			  {_State, _Times, _Siblings, _LogData, _OldState, CtrlMod} = invoke_light(Sequence, get_state, Pid),
    			  NewSiblingData = {Id, Pid,Sequence, CtrlMod,Location},
    			  %%NewSiblingData = {Id, Pid,Sequence, CtrlMod},
				  find_siblings_aux(Tail,LightsList, [NewSiblingData|CompleteSiblings])
    end.


light_final_format(LightsList) ->
	lists:map(fun({Id, Pid,Sequence}) ->
		{_State, _Times, _Siblings, _LogData, _OldState, CtrlMod} = invoke_light(Sequence, get_state, Pid),
		Res = moduler:get_sensor(CtrlMod),
		case Res of
			{reply, Sensor} -> {Id, Pid, Sequence, Sensor};
			_Other			-> {Id, Pid, Sequence,null}
		end		
		end,
		LightsList
	).
    

%%text file initialization of map
get_lights([]) ->
	filemanager:get_data("/sources/prueba2.txt");
get_lights(SourceFile) ->
	filemanager:get_data(SourceFile).
	
get_lanes([]) ->
	filemanager:get_data("/sources/prueba.txt");
get_lanes(SourceFile) ->
	filemanager:get_data(SourceFile).

get_obs([]) ->
	filemanager:get_data("/sources/prueba.txt");
get_obs(SourceFile) ->
	filemanager:get_data(SourceFile).

%get_data(SourceFile) -> 
%% Get the working directory, set complete path y read all lines
%    {ok, Cwd} = file:get_cwd(),
%    Path = Cwd ++ SourceFile,
%    readlines(Path).


%% Separete lanes that will function as sources for new cars
%% using the list of lanes get the ones that are sources for cars to enter the area
get_source_lanes(AllocatedLanes) ->    
    get_source_lanes(AllocatedLanes, []).
get_source_lanes([], SourceLanes) ->
    SourceLanes;
get_source_lanes([{LaneId,Pid, _LightController,_Dir,_ConnectedLanes, source_lane, ProbData} | Tail], SourceLanes) ->
    get_source_lanes(Tail, [{LaneId,Pid, [], ProbData} | SourceLanes]);
get_source_lanes([{_LaneId,_Pid, _LightController,_Dir,_ConnectedLanes, _Isource_lane, _ProbData} | Tail], SourceLanes) ->
    get_source_lanes(Tail, SourceLanes).
    
%% Read files to get lanes/ lights config
%readlines(FileName) ->
%    {ok, Device} =  file:open(FileName, [read]),
%    Result = get_all_lines(Device, []),
%    lists:map(fun(X) -> 
%   	         Stripped = string:strip(X, right, $\n),
%   		 {ok, ItemsTokens, _} = erl_scan:string(Stripped ++ "."),
%		 {ok, Term} = erl_parse:parse_term(ItemsTokens),
%		 Term
%   	      end,
%   	      Result
%   	    ).

%get_all_lines(Device, Accum) ->
%    case io:get_line(Device, "") of
%        {error, Reason} -> Reason;
%        eof  	      -> file:close(Device), lists:reverse (Accum);
%        Line 	      -> get_all_lines(Device, [Line|Accum])      
%    end.
   
%% Write down the results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop({Intersections, SourceLanes, {LogPath, DataLogPath, CheckPointLog}}, Time) ->
  %% On EVERY intersection in the system
  %% get the light_fsm and run each simulation for it
  %% according to the state of the FSM it will or will not move  the cars
  %% in case of a continue message run simulation, if stop abort program.
  receive
     {call, Pid, continue} ->
     	io:format("Running simulation iteration: ~w continue. Intersections ~w~n",[Time, Intersections]),
         %% Evaluate arrival of new cars to the lane
     	UpdatedSources = lane:estimate_new_arrival(SourceLanes, LogPath),
         
  %% Iterate intersection list and for each of them run evaluate state funciton
     	traverse_network(Intersections, Time),
     	write_result(LogPath, io_lib:format("Running simulation iteration: ~w continue. SourcesLanes ~w~n",[Time, SourceLanes])),               
	 	reply(Pid, {iteration, ok}),
	 	io:format("Finishing simulation iteration: ~w continue. Intersections~n",[Time]),
	 	io:format("----------------------------------------------------------~n~n"),
	 	write_result(LogPath, io_lib:format("Finishing simulation iteration: ~w continue. Intersections~n",[Time])),
	 	write_result(LogPath, io_lib:format("----------------------------------------------------------",[])),
	 	loop({Intersections, UpdatedSources, {LogPath, DataLogPath, CheckPointLog}}, Time + 1);    
    {call,Pid, stop} -> 
        tabulate_network(Intersections,DataLogPath),        
        reply(Pid, {normal, Time});
    {call, Pid, checkpoint} ->
    	safe_delete_files(CheckPointLog),
    	checkpoint(Intersections,SourceLanes, CheckPointLog),
    	reply(Pid, {ok, checkpoint}),
    	loop({Intersections, SourceLanes, {LogPath, DataLogPath, CheckPointLog}}, Time)
  end.


%% Add to a final list all the cars that completed theier travel 
%%add_to_outside(OutsideArea, Car) ->
%%   [Car | OutsideArea].
   
%% Receive traffic light list as param and evaluate state for ech one
traverse_network([], _Time) ->
    [];
traverse_network([{InterId, Intersection, Sequence, Sensor}|Intersections], Time) ->
    %%io:format("Evaluating state for light_fsm: ~w continue.~n",[InterId]),
    %%light_fsm:evaluate_state({InterId, Intersection, Time}),
    sensor:standby(Sensor),
    invoke_light(Sequence, evaluate_state,{InterId, Intersection, Time}),
    traverse_network(Intersections, Time).
    
%% Receive traffic light list as param and tabulates the results for ech one
tabulate_network([], _DataLog) ->
    [];
tabulate_network([{_InterId, Intersection, Sequence, _Sensor}|Intersections],DataLog) ->
    %%io:format("Evaluating state for light_fsm: ~w continue.~n",[InterId]),
    %%light_fsm:tabulate_data(Intersection, DataLog),
    invoke_light(Sequence, tabulate_data,{Intersection, DataLog}),
    tabulate_network(Intersections, DataLog).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     END  MAIN          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate random obstrucctions on streets
%%create_obstruction(LaneCap) ->
%%    {A1,A2,A3} = now(), 
%%    random:seed(A1, A2, A3),
%%    Begin = random:uniform(LaneCap - 3),
%%    End = Begin + random:uniform(1),
%%    io:format("Generating Obstruction: ~w~n",[{test, Begin, End}]),
%%    {test, Begin, End}.
    
%%allocate obstructions on lanes
allocate_obs(ObsRaw, TargetLaneId) ->
   allocate_obs(ObsRaw, TargetLaneId, []).
allocate_obs([], _TargetLaneId, ObsList) ->
   lists:reverse(ObsList);
allocate_obs([{Obs, TargetLaneId, BeginPos, EndPos} | Tail] , TargetLaneId, ObsList) ->
   allocate_obs(Tail,TargetLaneId, [{Obs, BeginPos, EndPos} | ObsList]);
allocate_obs([_NotMatchObs | Tail] , TargetLaneId, ObsList) ->
   allocate_obs(Tail, TargetLaneId, ObsList).
 

%%FUNCTION to calculate the max movement posible at the given speed
%%INPUT: MaxSpeed km/h that the car will reach at some point
%%	 CarAvgLegnth average length of cars default is 4.4 meters
%%OUTPUT: Max Positions that a car can move per second
max_car_move(MaxSpeeds, CarAvgLength) when length(MaxSpeeds) == 0 ->
	MaxSpeedMS = (40 * 1000) / 3600,
    MaxPos = MaxSpeedMS / CarAvgLength,
    [{all, erlang:round(MaxPos)}];
max_car_move(MaxSpeed, CarAvgLength) ->
    %MaxSpeedMS = (MaxSpeed * 1000) / 3600,
    %MaxPos = MaxSpeedMS / CarAvgLength,
    %erlang:round(MaxPos).
    lists:map(fun({Target, Speed}) -> {Target, erlang:round(((Speed * 1000) / 3600)/CarAvgLength)} end, MaxSpeed).

%%FUNCTION to get the determined MAX movement for the lane according to parameters
%%INPUT: LaneId : id of the lane (ex: lane212av1)
%%		 Dir	: av or ca
%%		 Speeds : List of all speeds to find the one
%%OUTPUT: Max movement (spaces) allowed per second
find_max_car_move(_LaneId, _Dir, MaxSpeeds) when length(MaxSpeeds) == 1 ->
	{all, Value} = lists:keyfind(all, 1, MaxSpeeds),
	Value;
find_max_car_move(LaneId, Dir, MaxSpeeds) ->
	S = atom_to_list(LaneId),
	SDir = atom_to_list(Dir),
	ParentLaneId1 = list_to_atom(string:concat(SDir, string:substr(S,5,1))),
    ParentLaneId2 = list_to_atom(string:concat(SDir, string:substr(S,5,3))),
    Value = try_find_element([ParentLaneId1, ParentLaneId2], MaxSpeeds),
    Value.

try_find_element([], MaxSpeeds) ->
	{all, Value} = lists:keyfind(all, 1, MaxSpeeds),
	Value;
try_find_element([Target | Matches], MaxSpeeds) ->
	Element = lists:keyfind(Target, 1, MaxSpeeds),
	case Element of
		false ->	try_find_element(Matches, MaxSpeeds);
		_Other ->	{Target, Value} = Element,
					Value
	end.

%%%%%%%%%
%% Checkpoint
%%DESC: calls to all 
%%INPUT: LanesList a list with all lanes to be saved in 
%%OUTPUT: None
checkpoint(Intersections, Sources, CheckPointLog) ->
	{LightChk, LanesChk, CarsChk, OCarsChk, SCarsChk} = CheckPointLog,
	checkpoint_lights(Intersections, {LightChk, LanesChk, CarsChk, OCarsChk}),
	checkpoint_sources(Sources, SCarsChk).

checkpoint_sources(Sources, CheckFile) ->
	lane:checkpoint_sources(Sources, CheckFile).

checkpoint_lights([], _CheckPointLog) ->
    [];
checkpoint_lights([{_InterId, Intersection, Sequence, _Sensor} | Intersections], CheckPointLog) ->
	%%light_fsm:checkpoint_data(Intersection, CheckPointLog),
	invoke_light(Sequence, checkpoint_data,{Intersection, CheckPointLog}),
    checkpoint_lights(Intersections, CheckPointLog).    
    
format_checkpointlogs(CheckLogs) ->
	L = tuple_to_list(CheckLogs),
	list_to_tuple(lists:map(fun(Log) -> {ok, Cwd} = file:get_cwd(), Cwd ++ Log end, L)).
	
restore_map({LightSource, LaneSource, ArrivaLog, DataLog, CheckPointLog, AvgCar}) ->
%%Restore configuration from files
    Axis = get_lights(LightSource),
    Roads = get_lanes(LaneSource),
    
    io:format("Allocating lights ~w.~n",[Axis]),
    {_Origin, AllocatedLights} = allocate_lights({Axis,[]}),
%%Get geometric distribution for each lane that has a type 2 (2 directions)
    NewRoads = lane:init_geometric(Roads),    
    
    %%get the max move for each car on the area
    MaxSpeedMove = max_car_move([], AvgCar),
    
%%Spawn every lane as a proccess    
    {_OrigLanes, AllocatedLanes} = allocate_lanes({NewRoads,[],[],MaxSpeedMove}),
%%Set archive_log for cars arrival to sources_lanes  
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/logs/",
    io:format("PATH ~s.~n",[Path]),    

    NewPath = Path ++ ArrivaLog,
    DataPath = Path ++ DataLog,
        
    file:delete(NewPath), %% delete old arrival log
    file:delete(DataPath), %% delete old data log
    
%%Connect each lane with siblings or any other connected lane
%%after that get sources lanes (lanes where cars will arrive to the area)    
    connect_lanes({AllocatedLanes,AllocatedLanes}),
    SourceLanes = get_source_lanes(AllocatedLanes),

	{LightsFile, LanesFile, CarsFile, OCarFile, SCarsChk} = CheckPointLog,
	%%RESTORE LANES DATA FROM CHECKPOINT
	lane:restore(AllocatedLanes, LanesFile, CarsFile, OCarFile),
	
	%%Get poisson dist for each source lane
    NewSources = lane:init_poisson(SourceLanes),   
    RestoredSources = lane:restore_sources(NewSources, SCarsChk),
	
	NewCheckLogPath = format_checkpointlogs(CheckPointLog),
		     
    %%io:format("SOURCES With poisson ~w.~n",[NewSources]),    
    io:format("PREPARING LightsFSM .~n",[]),
%%Spawn FSM for traffic lights on each intersection and then connect them    
    LightsFSM = connect({AllocatedLights, AllocatedLanes, [],Path}, restore), 
    io:format("LightsFSM ~w.~n",[LightsFSM]),
    set_connection_to_light_siblings(LightsFSM,LightsFSM),
	
	FinalLightsFSM = light_final_format(LightsFSM),
	
	restore_lights(LightsFSM, LightsFile),	
	
    {FinalLightsFSM, RestoredSources, {NewPath,DataPath, NewCheckLogPath}}.

restore_lights(Lights, LightFile)->
	LightsData =filemanager:get_data(LightFile),
	restore_lights_aux(Lights, LightsData).

restore_lights_aux([], _LightsData) ->
	{ok, restore_lights};
restore_lights_aux( [{InterId, Intersection, Sequence} | Tail], LightsData) ->
	RestoredData = safe_restore_lights(InterId,LightsData),
	invoke_light(Sequence, restore,{Intersection, RestoredData}),
	restore_lights_aux(Tail, LightsData).

safe_restore_lights(Id, List) ->
	Res = lists:keyfind(Id, 1, List),
	case Res of
		false -> [];
		_Other -> Res
	end. 

safe_delete_files(CheckLogs) ->
	L = tuple_to_list(CheckLogs),
	list_to_tuple(lists:map(fun(Log) -> file:delete(Log), {del, Log} end, L)).
