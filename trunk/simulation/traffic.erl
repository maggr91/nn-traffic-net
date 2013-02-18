-module(traffic).
-export([start/0, stop/0, light/4, lane/3, get_lights/0,get_lanes/0, set_map/0, connect/1, connect_lanes/1]).
-export([init/0]).

%% This module is used to run the simulation
%% of normal traffic, with no modification of ANN

start() ->
    register(traffic, spawn(traffic, init, [])).
 
stop() -> 
    call(stop).

call(Message) ->
    traffic ! {call, self(), Message},
    receive
       {reply, Reply} -> Reply
    end.
  
init() ->
    Ligths = set_map(), 
    loop(Ligths, 0).

%% set the network of traffic
set_map() ->
    Axis = get_lights(),
    Roads = get_lanes(),
    {_Origin, AllocatedLights} = allocate_lights({Axis,[]}),
    {_OrigLanes, AllocatedLanes} = allocate_lanes({Roads,[]}),
  
    connect({AllocatedLights, AllocatedLanes}),
    connect_lanes({AllocatedLanes,AllocatedLanes})
    AllocatedLights.
    
    
%% Spawn every intersection and lane on the map
allocate_lights({[],Spawned})->
    {[],Spawned};
allocate_lights({[{LightId,ManagedLanes, State, On_time, Go_time}|Tail], Spawned}) ->
    Pid = spawn(traffic,light, [ManagedLanes,State, On_time, Go_time]),
    allocate_lights({Tail, [{LightId,Pid} | Spawned]}). 
          
allocate_lanes({[], Spawned}) ->
    {[],Spawned};
allocate_lanes({[{LaneId,LightController,Dir, Type, ConnectedLanes, CarsQueque}|Tail], Spawned}) ->
    Pid = spawn(traffic,lane, [Type, [], CarsQueque]),
    allocate_lanes({Tail, [{LaneId,Pid, LightController,Dir,ConnectedLanes} | Spawned]}).

%%connect lights with its lanes
connect({[], LaneList}) -> 
    {[],{ok, ready}};
connect({[{LightId,Pid} | TailLight], LaneList}) -> 
    io:format("Luz: ~w / lineas: ~w.~n",[{LightId,Pid}, LaneList]),
    RemLanes = find_lanes({LightId,Pid}, LaneList, []),
    io:format("RemLanes: ~w.~n~n",[RemLanes]),
    connect({TailLight, RemLanes}).

find_lanes({LightId,LightPid}, [], RemLanes) -> 
    io:format("Exit ~w / sin lineas. Rem Lanes: ~w~n~n",[{LightId,LightPid},RemLanes]),
    RemLanes;
find_lanes({LightId,LightPid}, [{_LaneId,LanePid, LightId,Dir,_AdjLanes}| Tail], RemLanes) ->
    %%HeadAdd = {LaneId,LanePid, LightId},
    io:format("Coincidencia: ~w envio de mensaje.~n",[{LightId,LightPid}]),
    LightPid ! {connect_lane, LanePid, self(),Dir},
    io:format("Coincidencia: mensaje enviado.~n",[]),
    receive
        {reply, error} -> error;
  	{reply, ok} -> 
            io:format("Coincidencia: mensaje recibido.~n~n",[]),
  	    find_lanes({LightId,LightPid},Tail, RemLanes)
    end;
  
find_lanes({LightId, LightPid}, [LHead | LTail], RemLanes) ->
    io:format("No Coincidencia: ~w continue.~n",[{LightId,LightPid}]),
    find_lanes({LightId, LightPid}, LTail, [LHead|RemLanes]).


%%% Connect each lane with other near lanes
connect_lanes({[], LaneList}) -> 
    {[],{ok, ready}};
connect_lanes({[{LaneId,Pid, _Light,_Dir, AdjLanes} | TailLane], LaneList}) -> 
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
find_adjLanes({LaneId,LanePid}, AdjLaneId, [{AdjLaneId,AdjLanePid, _Light,_Dir, _AdjLanes}|_Tail]) ->
    io:format("Coincidencia: ~w envio de mensaje.~n",[{LaneId,LanePid}]),
    LanePid ! {connect_lane, AdjLanePid, self()},
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
        
%%Hard coded initialization of map
get_lights() -> 
    {ok, Cwd} = file:get_cwd(),
    %%io:format("Directorio: ",Cwd),
    Path = Cwd ++ "/sources/prueba2.txt",
    readlines(Path).
 
get_lanes() ->
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ "/sources/prueba.txt",
    readlines(Path).

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
   %%try get_all_lines(Device)
   %%   after file:colse(Device)
   %% end.

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        {error, Reason} -> Reason;
        eof  	      -> file:close(Device), lists:reverse (Accum);
        Line 	      -> get_all_lines(Device, [Line|Accum])      
    end.
   
%% Escribir resultados
write_result(Data, Path) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [Data])).
    
add_car(Position, Speed, Acc, Wait, Delay) ->
    {car,Position, Speed, Acc, Wait, Delay}.

loop(Intersections, Time) ->
  %% Para cada interseccion en el sistema
  %% saco cada semaforo que la forma y corro las simulaciones en estos
  %% sin importar si estan en ocupado o libre tiene que correrse algo
  
  receive
    {call, Pid, continue} ->    	  
	  UpdateIntersections = 
	  	lists:foreach(fun(Intersection)->
		 		 {IntersectionId,_Av, _Ca, Lights} = Intersection,
		 		 lists:foreach(fun(Light)->
		 		 		  {LightId,ManagedLanes, State, Go_time, On_time} = Light,	 		 		    
		 		 		  LightId ! {connection, self(), State}
		 		 	       end,
		 		 	       Lights
		 		 	      )
			      end,
			      Intersections
			     ),
	  reply(Pid, {iteration, ok}),
	  loop(UpdateIntersections, Time + 1);
    {call,Pid, stop} -> {finish, Time}
  end.

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

light(ManagedLanes, State, Go_time, On_time) ->
    receive
        {connection, Pid, active} -> 
    		NewState = update_onActive(ManagedLanes, Go_time, On_time, self(), State),
    		light(ManagedLanes, NewState, Go_time, On_time + 1);
        {connection, Pid, idle}   ->
		update_onIdle(ManagedLanes, Go_time, On_time, self(), State),
    		light(ManagedLanes, idle, Go_time, On_time + 1);
        {connect_lane, Lane_Pid, Pid, Dir}  -> 
    		{Av, Ca} = ManagedLanes,
    		io:format("{Av ~w, Ca: ~w.~n",[Av, Ca]),
    		if
    		  Dir =:= av -> 
    		  	NewManaged = {[Lane_Pid| Av], Ca},
    		  	reply(Pid,ok),
    		  	io:format("light ~w, lanes: ~w.Dir:~w~n",[self(), NewManaged, Dir]),
    		  	light(NewManaged, State, Go_time, On_time);
    		  Dir =:= ca ->
    			NewManaged = {Av, [Lane_Pid| Ca]},
    			reply(Pid,ok),
    			io:format("light ~w, lanes: ~w. Dir:~w~n ",[self(), NewManaged,Dir]),
    		  	light(NewManaged, State, Go_time, On_time);
     	  	  true ->
     	  	  	io:format("NO TYPE MATCH",[]),
     	  	  	reply(Pid,ok), 
     	  	  	light(ManagedLanes, State, Go_time, On_time)     	  	  	
     	  	end;
        stop ->	      
    		{ok, normal}
    end.
  
update_onActive(Lanes, Go_time, On_time, Pid, State) ->  
%% ver como actualizar los valores ver si requiere ser recursivo o proceso
    lists:foreach(fun(LaneId)->
		   %%{LaneId,Type, _Cars} = Lane,	 		 		    
			LaneId ! {go, Pid, On_time}
			end,
		Lanes
 	       ).
update_onIdle(Lanes, Go_time, On_time, Pid, State) ->  
%% actualizar todos los carros en espera
    lists:foreach(fun(LaneId)->
		   %%{LaneId,Type, _Cars} = Lane,	 		 		    
			LaneId ! {waiting, Pid, On_time}
			end,
		Lanes
 	       ).
  
  
lane(Type, ConnectedLanes, CarsQueque) ->
  %% For each lane on every street in the system, 
  %% get the move acording to the ligth state passed 
  %% and run the complete simulation
    receive
        {go, LightController, Time} -> %% if a go message is received it tells all cars to start to move
		NewCarsQueque = move_cars(CarsQueque, LightController),
		lane(Type, ConnectedLanes, NewCarsQueque);
           
	{stop, LightController} ->  
	%% send a message to all cars to start to stop preventing them to pass the red ligth
		NewCarsQueque = stop_cars(CarsQueque,LightController),
		lane(Type, ConnectedLanes, NewCarsQueque);
      
	{dispatch, LightController, Time} ->		
	%% In case that the car reaches the end of the lane it has to be changed to 
	%% a new lane.
		NewCarsQueque = dispatch_cars(CarsQueque, ConnectedLanes, Time, LightController),
		lane(Type, ConnectedLanes, NewCarsQueque);
	{waiting, LightController, Time} ->
		NewCarsQueque = waiting(CarsQueque,Time),
		lane(Type, ConnectedLanes, NewCarsQueque);
	
	{connect_lane, AdjLane_Pid, Pid}  ->     		
    		io:format("{Connected Lanes ~w.~n",[ConnectedLanes]),
    		NewConnectedLanes = [AdjLane_Pid| ConnectedLanes],
     		io:format("{new Connected Lanes ~w.~n",[NewConnectedLanes]),
    		reply(Pid,ok),
    		lane(Type, NewConnectedLanes, CarsQueque);  	  	
		
	stop -> {ok, normal}
	%%{connect_light, Light_Pid} ->
	%%	lanes(Light_Pid, Type, ConnectedLanes, CarsQueque)	
    end.  

move_cars([],_) -> [];
move_cars([FirstCar|Waiting], Pid) -> true.
    %%{go, Line1, Line2, Time} -> true.

stop_cars([], _) -> [];
stop_cars([FirstCar|Waiting], Pid) -> true.

dispatch_cars([], _ConnectedLanes, _Time, Pid) -> [];
dispatch_cars([FirstCar|Waiting], ConnectedLanes, Time, Pid) -> true.


waiting([],_) -> [];
waiting([FirstCar|Waiting], Pid) -> true.  
