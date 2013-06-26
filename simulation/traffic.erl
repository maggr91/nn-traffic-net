-module(traffic).

-export([start/0, set_map/0, connect/1, connect_lanes/1]).

-export([init/0]).

%-export([test_move_av/1,test_move_ca/1, test_idle/1]).

%-import(light_fsm,[init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%client calls
-import(light_fsm,[start_link/0,move_avenue/1, move_street/1, idle/1,update_siblings/2, evaluate_state/1]).


%% This module is used to run the simulation
%% of normal traffic, with no modification of ANN

%% Start method, USE this to run simulation
start() ->    
%%    register(poissonServer,prob:start({poisson,6})),    
%%    register(traffic, spawn(traffic, init, [])),
      spawn(traffic, init, []).
%%    timer:apply_after(400, traffic, run_simulation, [Lapse]).
    %%run_simulation(Lapse).

%%run aux
%%run_simulation(Lapse) ->    
%%    run_simulation(Lapse, 0).

%% Loop used to run each iteration of the simulation
%%run_simulation(Lapse, Current) when Current < Lapse ->
%%    io:format("CONTINUING with run.~n",[]),
%%    call(continue),   
%%    run_simulation(Lapse, Current + 1);
%%run_simulation(Lapse, Current) when Current >= Lapse ->
%%    stop().
 
%%stop() -> 
%%    call(stop).

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

%%call(Message) ->
%%    io:format("CALLING PROCESS LOOP.~n",[]),
%%    traffic ! {call, self(), Message},
%%    receive
%%       {reply, Reply} -> Reply
%%    end.

init() ->
    MapData = set_map(), 
    loop(MapData, 0).

%% Function to load the map by reading files located in
%% ~/sources folder
set_map() ->
%%Read data from files
    Axis = get_lights(),
    Roads = get_lanes(),
    Obs = get_obs(),

    io:format("Allocating lights ~w.~n",[Axis]),
    {_Origin, AllocatedLights} = allocate_lights({Axis,[]}),
%%Get geometric distribution for each lane that has a type 2 (2 directions)
    NewRoads = lane:init_geometric(Roads),
    io:format("FULL roads ~w.~n",[NewRoads]),
%%Spawn every lane as a proccess    
    {_OrigLanes, AllocatedLanes} = allocate_lanes({NewRoads,[],Obs}),
%%Set archive_log for cars arrival to sources_lanes  
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/logs/",
    io:format("PATH ~s.~n",[Path]),
    NewPath = Path ++ "arrival_log.txt",
    DataPath = Path ++ "data_log.txt",
%%Connect each lane with siblings or any other connected lane
%%after that get sources lanes (lanes where cars will arrive to the area)    
    connect_lanes({AllocatedLanes,AllocatedLanes}),
    SourceLanes = get_source_lanes(AllocatedLanes),

%%Get poisson dist for each source lane
    NewSources = lane:init_poisson(SourceLanes),    
    %%io:format("SOURCES With poisson ~w.~n",[NewSources]),    

%%Spawn FSM for traffic lights on each intersection and then connect them    
    LightsFSM = connect({AllocatedLights, AllocatedLanes, [],Path}), 
    set_connection_to_light_siblings(LightsFSM,LightsFSM),

    {LightsFSM, NewSources, {NewPath,DataPath}}.


%% DEPRECATED: Spawn every intersection and lane on the map
%% modify to avoid this step
allocate_lights({[],Spawned})->
    {[],Spawned};
allocate_lights({[{LightId,_ManagedLanes, Siblings, Times}|Tail], Spawned}) ->
    %%Pid = spawn(traffic,light, [ManagedLanes,Siblings, Cycle_time, Go_time]),
    %%Replace Go_time for  AllRed_time (Go_time set by default to 0
    %%allocate_lights({Tail, [{LightId,Siblings, Cycle_time,Go_time, AllRed_time} | Spawned]}). 
    allocate_lights({Tail, [{LightId,Siblings, Times} | Spawned]}). 
 
 
%% Spawn every lane on the area with the respective data         
allocate_lanes({[], Spawned,_Obs}) ->
    {[],Spawned};
allocate_lanes({[{LaneId,LightController,Dir, Type, ConnectedLanes, 
                  CarsQueque, IsSource, Capacity, {probList, ProbData}, GeoProb}|Tail], Spawned, Obs}) ->
%%for each lane in the list spawn a proccess
    %%Pid = lane:start({Type, [], CarsQueque, Capacity, [], ProbData, GeoProb}),
    %%Obstruction = [create_obstruction(Capacity)],
    io:format("Obstacules loaded: ~w",[Obs]),
    Obstruction = allocate_obs(Obs, LaneId),  
    Pid = lane:start({LaneId, Type, [], CarsQueque, Capacity, Obstruction, GeoProb}),
    {arrival, ProbList} = lists:keyfind(arrival, 1, ProbData),
    ProbRanges = list_to_tuple(ProbList),
    allocate_lanes({Tail, [{LaneId,Pid, LightController,Dir,ConnectedLanes, IsSource, ProbRanges} | Spawned], Obs}).

%%connect lights with its lanes
connect({[], _LaneList, LightsFSM,_LogData}) -> 
    LightsFSM;
connect({[{LightId,Siblings, Times} | TailLight], LaneList,LightsFSM, LogData}) -> 
    %%io:format("Luz: ~w / lineas: ~w.~n",[LightId, LaneList]),
    {ManagedLanes, RemLanes} = find_lanes({LightId, {[],[]}}, LaneList, []),
    %%io:format("Managed Lanes: ~w.~n~n",[ManagedLanes]),
    PathLog = (LogData ++ lists:flatten(io_lib:format("~p",[LightId]))) ++ ".txt",
    {ok, LightPid} = light_fsm:start_link({LightId, ManagedLanes,Siblings, Times, PathLog}),
    connect({TailLight, RemLanes, [{LightId,LightPid}|LightsFSM], LogData}).


%% Find all lanes that are connected to the LIght passed as param
find_lanes({_LightId,ManagedLanes}, [], RemLanes) -> 
    %%io:format("Exit ~w / sin lineas. Rem Lanes: ~w~n~n",[{LightId,ManagedLanes},RemLanes]),
    {ManagedLanes, RemLanes};
find_lanes({LightId,{Av, Ca}}, [{LaneId,LanePid, LightId,Dir,_AdjLanes, _IsSource, _ProbData}| Tail], RemLanes) ->
    %%io:format("Coincidencia: ~w envio de mensaje.~n",[{LightId,LaneId}]),
    %%io:format("{Av ~w, Ca: ~w.~n",[Av, Ca]),
    if  %%Determine lane type if its either a street or an avenue and locate it in the corresponding list
        Dir =:= av -> 
    	    NewManaged = {[{LaneId,LanePid}| Av], Ca},    	    
    %%	    io:format("light ~w, lanes: ~w.Dir:~w~n",[self(), NewManaged, Dir]),
    %%	    io:format("Coincidencia: mensaje enviado.~n",[]),
    %%      io:format("Coincidencia: mensaje recibido.~n~n",[]),
            find_lanes({LightId,NewManaged},Tail, RemLanes);
    	Dir =:= ca ->
    	    NewManaged = {Av, [{LaneId,LanePid}| Ca]},
   %% 	    io:format("light ~w, lanes: ~w. Dir:~w~n ",[self(), NewManaged,Dir]),
   %% 	    io:format("Coincidencia: mensaje enviado.~n",[]),
   %%       io:format("Coincidencia: mensaje recibido.~n~n",[]),
            find_lanes({LightId,NewManaged},Tail, RemLanes);
        true ->
     	    io:format("NO TYPE MATCH",[])     	    
    end; 
find_lanes({LightId, ManagedLanes}, [LHead | LTail], RemLanes) ->
    %%io:format("No Coincidencia: ~w continue.~n",[{LightId,ManagedLanes}]),
    find_lanes({LightId, ManagedLanes}, LTail, [LHead|RemLanes]).


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
    %%io:format("Exit ~w / sin lineas.~n~n",[{LaneId,LanePid}]),
    [];
set_connection_to_lane({LaneId,LanePid}, [{Type, AdjLaneTypeList} | AdjLanesTypeTail], LaneList) ->    
    AdjLaneTypeData = iterate_Adj_lane_types(AdjLaneTypeList,LaneList,[]),
    %%io:format("Coincidencia: ~w envio de mensaje.~n",[{LaneId,LanePid}]),
    LanePid ! {connect_lane, {Type, AdjLaneTypeData}, self()},
    %%io:format("Coincidencia: mensaje enviado.~n",[]),
    receive
        {reply, error} -> error;
  	{reply, ok} -> 
            %%io:format("Coincidencia: mensaje recibido. Linea agregada~n~n",[]),
            set_connection_to_lane({LaneId,LanePid},AdjLanesTypeTail, LaneList)
    end.   

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
set_connection_to_light_siblings([{_Id, Pid}|LightsRem], LightsList) ->
    %%{_State, _Cycle_time, _Go_time, Siblings, _LogData} = light_fsm:get_state(Pid),
    {_State, _Times, Siblings, _LogData} = light_fsm:get_state(Pid),
%% The siblings list is a list of tuples that contaings LightId use this to find the corresponding PID
    CompleteSiblings = find_siblings(Siblings,LightsList,[]),
%% After getting the list with all PIDs, update the corresponding FSM    
    light_fsm:update_siblings(Pid, CompleteSiblings),
    set_connection_to_light_siblings(LightsRem,LightsList).
    
%% Iterate sibling's list and find the corresponding data in the lights_FSM list
%% for this we use the lists:keyfind/3 to return a tuple {LightId, LightPid}
find_siblings([],_LightsList, CompleteSiblings) ->
    lists:reverse(CompleteSiblings);
find_siblings([SiblingId|Tail], LightsList,CompleteSiblings) ->
    SiblingData = lists:keyfind(SiblingId, 1, LightsList), 
    find_siblings(Tail,LightsList, [SiblingData|CompleteSiblings]).
    

%%text file initialization of map
get_lights() -> 
%% Get the working directory, set complete path y read all lines
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/sources/prueba2.txt",
    readlines(Path).
 
get_lanes() ->
%% Get the working directory, set complete path y read all lines
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/sources/prueba.txt",
    readlines(Path).

get_obs() ->
%% Get the working directory, set complete path y read all lines
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/sources/obs.txt",
    readlines(Path).

%get_statistics() ->
%    {ok, Cwd} = file:get_cwd(),
%    Path = Cwd ++ "/sources/prob.txt",
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

loop({Intersections, SourceLanes, {LogPath, DataLogPath}}, Time) ->
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
	 loop({Intersections, UpdatedSources, {LogPath, DataLogPath}}, Time + 1);    
    {call,Pid, stop} -> 
         tabulate_network(Intersections,DataLogPath),
         reply(Pid, {normal, Time})
  end.


%% Add to a final list all the cars that completed theier travel 
add_to_outside(OutsideArea, Car) ->
   [Car | OutsideArea].
   
%% Receive traffic light list as param and evaluate state for ech one
traverse_network([], _Time) ->
    [];
traverse_network([{InterId, Intersection}|Intersections], Time) ->
    %%io:format("Evaluating state for light_fsm: ~w continue.~n",[InterId]),
    light_fsm:evaluate_state({InterId, Intersection, Time}),
    traverse_network(Intersections, Time).
    
%% Receive traffic light list as param and tabulates the results for ech one
tabulate_network([], _DataLog) ->
    [];
tabulate_network([{_InterId, Intersection}|Intersections],DataLog) ->
    %%io:format("Evaluating state for light_fsm: ~w continue.~n",[InterId]),
    light_fsm:tabulate_data(Intersection, DataLog),
    tabulate_network(Intersections, DataLog).    



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
    
%% Estimate initial Cars arrival for all sources lanes
%%init_poisson(SourcesLanes) ->
%%    io:format("LOOKING FOR POISSON ~n",[]),
%%    init_poisson(SourcesLanes, []).
%%init_poisson([],SourceUpdated) ->
%%    io:format("POISSON ENDED ~n",[]),
%%    SourceUpdated;
%%init_poisson([{LaneId, LanePid, [], ProbData}| Tail], SourceUpdated) ->
%%    Arrival = data_distribution(),
%%    io:format("Arrival time ~w~n",[Arrival]),
%%    NewLane = {LaneId, LanePid, [],ProbData, Arrival},
%%    init_poisson(Tail, [NewLane | SourceUpdated]).
    
%% Calculate statistics
%% use the module statistics to calculate cars arrival and other scenaries
%%data_distribution() ->
    %N = random:uniform(12),
    %Lambda= 6,
    %{N, lanes_poisson(N, Lambda)}.
%%    poissonServer ! {valor, self()},
%%    receive
%%  	{reply, Arrival} -> io:format("Valor recibido ~w~n",[Arrival]),
%%    		   	 Arrival;
%%    	Other	      -> io:format("Error calling the server: ~w~n",[Other])
%%    end.
