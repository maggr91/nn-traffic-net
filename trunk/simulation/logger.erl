-module(logger).
-export([start/0, debug_sm/2, debug_ann/2, debug_lane/2, stop/1]).

-export([init/0]).

start() ->
	filelib:ensure_dir("logs/"),
	spawn(?MODULE, init, []).
	
init() ->
	Files = [{simulation, "/logs/simulation_log.txt"}, {ann, "/logs/ann_log.txt"}, {lane, "/logs/lanes_log.txt"}],
	{ok, Cwd} = file:get_cwd(),
	
	Logs = lists:map(fun({Id, File}) -> {Id, Cwd ++ File} end, Files),
	%io:format("FILES READY ~p",[Logs]),
	io:format("Proceso iniciado ~p~n~n",[Logs]),
	listen(Logs).
	
listen(Logs) ->
	receive
		{log, Type, Data} -> %io:format("Lookgin for ~w in ~w", [Type, Logs]),
							 {Type, File} = lists:keyfind(Type, 1, Logs),
							 %io:format("File ~p", [File]),
							 filemanager:write_result(File, Data),
							 listen(Logs);
		stop -> {normal, ?MODULE};
		status -> io:format("Test ~n")
	end.


debug_sm(Logger, Data) ->
	Logger ! {log, simulation, Data}.
	
debug_ann(Logger, Data) ->
	%io:format("Log ~w ann data ",[Logger]),
	Logger ! {log, ann, Data}.
	
debug_lane(Logger, Data) ->
	Logger ! {log, lane, Data}.
	
stop(Logger) ->
	IsReg = whereis(Logger),	
	if IsReg =:= undefined ->
		try
			Logger ! stop
		catch 		
			Exception:Reason -> io:format("Logger already stopped, continue. Exception ~p , Reason ~p ~n",[Exception, Reason]),
								continue
    	end;		
	   true ->
	   	ok
	end.
