-module(simulation).

-export([start/2, run_simulation/1, restore/0, call/1]).


start(Lapse, MaxSpeed) ->
    %%MaxSpeed = 50, 
    [Config | _Junk] = get_config(),
    start_probServers(),
    register(trafficServer, traffic:start(normal, {MaxSpeed, Config})),
    timer:apply_after(400, simulation, run_simulation, [{normal, Lapse, now()}]).
  
restore() ->
	start_probServers(),
	RestoredStruct = load_checkpoint(filelib:is_dir("checkpoint")),
	io:format("restore data ~w",[RestoredStruct]),
	{ConfigData, LastTime, TopTime} = RestoredStruct,
	register(trafficServer, traffic:start(restore, {ConfigData, LastTime})),
	
    timer:apply_after(400, simulation, run_simulation, [{restore, LastTime, TopTime, now()}]).

%%run simulation from last checkpoint
run_simulation({restore, LastTime, Lapse, BeginTime}) ->    
    run_simulation(Lapse, LastTime, BeginTime);

%%run aux
run_simulation({normal, Lapse, BeginTime}) ->    
    run_simulation(Lapse, 0, BeginTime).

%% Loop used to run each iteration of the simulation
run_simulation(Lapse, Current, BeginTime) when Current < Lapse ->
	CurrentTime = now(),
	case microsec_to_minutes(timer:now_diff(CurrentTime,BeginTime)) >= 1 of
		false ->
				io:format("CONTINUING with run.~n",[]),
				call(continue),   
				run_simulation(Lapse, Current + 1, BeginTime);
		true ->
				io:format("SAVING CHECKPOINT.~n",[]),
				checkpoint(Current, Lapse),
				%%timer:apply_after(300, simulation, call, [continue]),
				call(continue),
				run_simulation(Lapse, Current + 1, now())
	end;
				
run_simulation(Lapse, Current, _BeginTime) when Current >= Lapse ->
	io:format("FINAL SAVING CHECKPOINT.~n",[]),
	checkpoint(Current, Lapse),
    stop().
    %%checkpoint(Current, Lapse).
    %%timer:apply_after(300, simulation, unregister_process, []). 
stop() -> 
	stop_probServers(),
    call(stop).

%%unregister_process() ->
%%    unregister(poissonServer),
%%    unregister(geoServer),
%%    unregister(geoCedServer),
%%    unregister(trafficServer).

start_probServers() ->
	register(poissonServer,prob:start({poisson,6})),
    register(geoServer,prob:start({geometrica,0.5})),
    register(geoCedServer,prob:start({geometrica,0.25})),
    register(trainerServer, trainer:start()).

stop_probServers() ->
    poissonServer ! killyou,
    geoServer ! killyou,
    geoCedServer ! killyou,
    trainerServer ! killyou.
    
call(Message) ->
    io:format("CALLING PROCESS LOOP.~n",[]),
    trafficServer ! {call, self(), Message},
    receive
       {reply, Reply} -> Reply
    end.

get_config() ->
	filemanager:get_data("/config.txt").
%% Get the working directory, set complete path y read all lines
%%    {ok, Cwd} = file:get_cwd(),
%%    Path = Cwd ++ "/config.txt",
%%    io:format("Path ~p.~n",[Path]),
%%    readlines(Path).

%% Read config file
%%readlines(FileName) ->
%%    {ok, Device} =  file:open(FileName, [read]),
%%    Result = get_all_lines(Device, []),
%%    lists:map(fun(X) -> 
%%   	         Stripped = string:strip(X, right, $\n),
%%   		 {ok, ItemsTokens, _} = erl_scan:string(Stripped ++ "."),   		 
%%		 {ok, Term} = erl_parse:parse_term(ItemsTokens),
%%		 Term
%%   	      end,
%%   	      Result
%%   	    ).

%%get_all_lines(Device, Accum) ->
%%    case io:get_line(Device, "") of
%%        {error, Reason} -> Reason;
%%        eof  	      -> file:close(Device), lists:reverse (Accum);
%%        Line 	      -> get_all_lines(Device, [Line|Accum])      
%%    end.


%%find configuration
find_config_data(ConfigData, Key) ->
	Res = lists:keyfind(Key, 1, ConfigData),
	case Res of
		false -> [];
		{Key, Value} -> Value
	end.

microsec_to_minutes(Value) ->
	((((Value / 1000) / 1000) / 60)). 
%%%%%%%%%%%%%%%%%

clearLastCheck()->
	file:del_dir("checkpoint").
	
checkpoint(CurrentTime, Lapse) ->
	%%stop_probServers(),
	clearLastCheck(),
	filelib:ensure_dir("checkpoint/"),
	{{Y, M, D}, {H, Mi, _S}} = calendar:local_time(),
	filemanager:write_raw("checkpoint/Log.txt", io_lib:format("{Last checkpoint on :~p/~p/~p at ~p:~p, ~w}",[D,M,Y, H, Mi, CurrentTime])),
	filemanager:write_raw("checkpoint/TLog.txt", io_lib:format("[{lastTime,~w}, {topTime, ~w}]",[CurrentTime, Lapse])),
	call(checkpoint).
	
load_checkpoint(Exist) when Exist == true ->
	[ConfigData | _Junk] = get_config(),
	Checkpoint = lists:last(filemanager:get_data("/checkpoint/TLog.txt")),
	io:format("~w", [Checkpoint]),
	LastTime = find_config_data(Checkpoint, lastTime),
	TopTime = find_config_data(Checkpoint, topTime),
	{ConfigData, LastTime, TopTime};
	
load_checkpoint(Exist) when Exist == false ->
	io:format("There are no checkpoint files please provide a valid checkpoint dir").
