-module(traffic).
-export([start/0, stop/0, light/4, lane/3, connect/2, get_lights/0,get_lanes/0, set_map/0]).

%% This module is used to run the simulation
%% of normal traffic, with no modification of ANN

start() ->
  register(traffic, spawn(traffic, init, [])).
 
stop() -> call(stop).

call(Message) ->
 traffic ! {call, self(), Message},
 receive
   {reply, Reply} -> Reply
 end.
  
init() ->
  Ligths = set_map(), 
  loop(Ligths, 0).

%% cargar todos los procesos
set_map() ->
  Axis = get_lights(),
  Roads = get_lanes(),
  %%lists:foreach(fun ({LightId,ManagedLanes, State, On_time, Go_time}) ->
  %%		    lists:filter( fun ({_lane, Connection,Dir, Type, Cars}) ->
  %%		    			LightId =:= Connection
  %%		    			if
  %%		    			  Dir =:= 
  %%		    		  end,
  %%		    		  Roads
  %%		    		)
  %%		end,
  %%		Axis
  %%	       ), 
  {Origin, Allocated} = allocate_lights({Axis,[]}),
  Allocated.

allocate_lights({[],Spawned})->
	{[],Spawned};
allocate_lights({[{LightId,ManagedLanes, State, On_time, Go_time}|Tail], Spawned}) ->
	Pid = spawn(traffic,light, [ManagedLanes,State, On_time, Go_time]),
	allocate_lights({Tail, [{LightId,Pid} | Spawned]}). 
          
allocate_lanes({[], Spawned}) ->
	{[],Spawned};
allocate_lanes({[{LaneId,LightController,Dir, Type, ConnectedLanes, CarsQueque}|Tail], Spawned}) ->
	Pid = spawn(traffic,lane, [Type, ConnectedLanes, CarsQueque]),
	allocate_lanes({Tail, [{LaneId,Pid, LightController} | Spawned]}).
	
	          
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


%% Leer los archivos para configuración de las calles
readlines(FileName) ->
   {ok, Device} =  file:open(FileName, [read]),
   Result = get_all_lines(Device, []),
%%   io:format("Resultado de archivo: ",[]),
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
      eof  	      -> file:close(Device), lists:reverse (Accum);
      Line 	      -> get_all_lines(Device, [Line|Accum]);
      {error, Reason} -> Reason
   end.
   
%% Escribir resultados
write_result(Data, Path) ->
  file:write_file(Path, io_lib:fwrite("~p.\n", [Data])).
    
add_car(Position, Speed, Acc, Wait, Delay) ->
 {car,Position, Speed, Acc, Wait, Delay}.

connect(Light_Pid, Lane_Pid) ->
  Light_Pid ! {connect_lane, Lane_Pid},
  Lane_Pid  ! {connect_light, Light_Pid}.
  
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
    {connection, Pid, Active} -> 
    		NewState = update_onActive(ManagedLanes, Go_time, On_time, self(), State),
    		light(ManagedLanes, NewState, Go_time, On_time + 1);
    {connection, Pid, Idle}   ->
		update_onIdle(ManagedLanes, Go_time, On_time, self(), State),
    		light(ManagedLanes, Idle, Go_time, On_time + 1);
    {connect_lane, Lane_Pid, Type}  -> 
    		{Av, Ca} = ManagedLanes,
    		if
    		  Type =:= av -> 
    		  	NewManaged = {[Lane_Pid| Av], Ca},
    		  	light(NewManaged, State, Go_time, On_time);
    		  Type =:= ca ->
    			NewManaged = {Av, [Lane_Pid| Ca]},
    		  	light(NewManaged, State, Go_time, On_time);
     	  	  true -> light(ManagedLanes, State, Go_time, On_time)
     	  	end;
    stop ->	      
    		{ok, idle}
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
		lane(Type, ConnectedLanes, NewCarsQueque)
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
