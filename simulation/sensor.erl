-module(sensor).

-export([start/1, get_records/2, car_pass/4, standby/1, change/2, idle/2, check_standby/2, get_records_for_sibling/2]).

-export([init/1]).


start(Args) ->
	spawn(sensor, init, [Args]).
	
init({normal, Args}) ->
	{Lanes, File} = Args,
	NewLanes = format_lanes(Lanes),
	%%NewFile = formated_log(File),
	sensor(NewLanes, File, 0);

init({restore, Args}) ->
	{Lanes, File} = Args,
	{RestoredLanes, RestoredStandBy} = restore(Lanes, File),
	sensor(RestoredLanes, File, RestoredStandBy).

sensor(Lanes, File, StandBy) ->
	receive 
		{update, _CallerPid, CallerId, Value, Dir} ->
			%%io:format("[SENSOR] Sensor msg ~w for lane: ~w with data: ~w~n", [self(), CallerId, {Dir, Value}]),
			NewLanes = update_count(CallerId, Lanes, Lanes, Value, Dir),			
			sensor(NewLanes, File, 0);
		{standby, _CallerPid} ->
			sensor(Lanes, File, StandBy + 1);
		{change, _CallerPid, Dir} ->
			%%io:format("[SENSOR] Change Sensor targets ~n", []),
			NewLanes = reset_count(Lanes, Dir,0),			
			sensor(NewLanes, File, StandBy);
		{idle, _CallerPid, Dir} ->
			%%io:format("[SENSOR] Idle Sensor targets ~n", []),
			NewLanes = reset_count(Lanes, Dir,-100),
			%%io:format("[SENSOR] Sensor new lanes count ~w~n",[NewLanes]),
			sensor(NewLanes, File, StandBy);
		{check_standby, CallerPid, Limit} ->
			Status = evaluate_standby(Limit, StandBy),
			reply(CallerPid, Status),
			sensor(Lanes, File, StandBy);
		{checkpoint, _CallerPid} ->
			filelib:ensure_dir("checkpoint/sensors/"),
			write_checkpoint(Lanes, File, StandBy),
			sensor(Lanes, File, StandBy);
		{update_file, NewFile} ->
			sensor(Lanes, NewFile, StandBy);
		{records, CallerPid, Dir, FlowDir} ->
			Data = records(Dir, Lanes, FlowDir),
			reply(reply_records, CallerPid, Data),
			sensor(Lanes, File, StandBy);
			
		{records_for_sibling, CallerPid, SourcesDirList} ->
			Data = records_for_sibling(SourcesDirList, Lanes),
			reply(reply_sensor_records, CallerPid, Data),
			sensor(Lanes, File, StandBy);
		{stop, _CallerPid} ->
			%reply(CallerPid, ok),
			{normal, sensor}
	end.
	
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.
    
reply(ReplyKey, Pid, Reply) ->
    Pid ! {ReplyKey, Reply}.
    
format_lanes(Lanes) ->
	lists:map(fun({Dir, List}) ->
		{Dir, lists:map( fun({LaneId, LanePid}) -> {LaneId, [{id, LanePid},{dsp_str, 0},{dsp_trn, 0}, {rain, 0}, {total_str, 0}, {total_trn, 0}, {total, 0}]} end, List)}
		end,
		Lanes
	).

update_count(_LaneId, [], OldLanes, _Value, _CounterId) ->
	OldLanes;
update_count(LaneId, [{Dir, LanesList} | Lanes], OldLanes, Value, CounterId) ->
	Exist = lists:keyfind(LaneId, 1, LanesList),
	case Exist of
		false  -> update_count(LaneId, Lanes, OldLanes, Value, CounterId);
		_Other -> {LaneId, Data} = Exist,
				  TotalCounterId = list_to_atom(string:concat(atom_to_list(total_), string:substr(atom_to_list(CounterId),5,3))),
				  NewData = update_data_count([{CounterId, Value}, {TotalCounterId, Value}, {total, Value}], Data),
				  NewLaneList = lists:keyreplace(LaneId,1, LanesList, {LaneId, NewData}),
				  lists:keyreplace(Dir,1, OldLanes, {Dir, NewLaneList})
	end.

reset_count(Lanes, Dir, ResetVal) ->
	DirLanes = find_element(Dir, Lanes),
	ResetLanes = lists:map(fun({LaneId, LaneData}) ->
		TmpList = lists:keyreplace(dsp_str, 1, LaneData, {dsp_str, ResetVal}),
		FinalList = lists:keyreplace(dsp_trn, 1, TmpList, {dsp_trn, ResetVal}),
		{LaneId, FinalList}
		end,
		DirLanes
	),
	lists:keyreplace(Dir, 1, Lanes, {Dir, ResetLanes}).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% GENERAL %%%%%%%%%%%%%%%%%
find_element(_Id, []) ->
	false;
find_element(Id, Data) ->
	Element = lists:keyfind(Id, 1, Data),
	case Element of
		false ->	[];
		_Other ->	{Id, Value} = Element,
					Value
	end.


%update_data([], NewData) ->
%	NewData;
%update_data([{ElementId, NewValue} | UpdateTail], OldData) ->
%	NewData = lists:keyreplace(ElementId,1, OldData, {ElementId, NewValue}),
%	update_data(UpdateTail, NewData).
	

%%update counts
update_data_count([], NewData) ->
	NewData;
update_data_count([{ElementId, NewValue} | UpdateTail], OldData) ->
	{ElementId, OldValue} = lists:keyfind(ElementId, 1, OldData),
	NewData = lists:keyreplace(ElementId,1, OldData, {ElementId, OldValue + NewValue}),
	update_data_count(UpdateTail, NewData).

evaluate_standby(Limit, StandBy) when StandBy >= Limit ->
	stop;
evaluate_standby(_Limit, _StandBy) ->
	continue.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checkpoint
write_checkpoint(Lanes, File, StandBy)->
	%%io:format("[SENSOR] CHECKPOINT SENSOR file : ~p ~n",[File]),
	FormatedLanes = format_for_check(Lanes, StandBy),
	lists:map(fun(Data) -> 
		filemanager:write_raw(File, io_lib:format("~w", [Data]))
		end,
		FormatedLanes),
	{normal, checkpoint}.

format_for_check(Lanes, StandBy) ->
	lists:append(
		lists:map(
			fun({Dir, List}) -> 
				lists:map(
					fun({LaneId, Data})-> 
						CheckData = lists:filter(fun({Key, _Value}) -> Key =/= id end, Data), 
						{Dir, LaneId, CheckData, StandBy} 
					end, 
					List
				) 
			end, 
			Lanes
		)
	).


restore(Lanes, File) ->
	%io:format("[SENSOR] SensorFileName to restore: ~p, lanes: ~w~n~n", [File, Lanes]),
	RestoredLanes = filemanager:get_data(File),
	[{_DirAux,_LaneIdA, _DataA, RestoredStandBy} | _Rest ] = RestoredLanes,
	{lists:foldl(
		fun({Dir,LaneId,Data, _StandBy}, Res) ->
			Exist = lists:keyfind(Dir, 1, Res),
			LanePidList = find_element(Dir, Lanes),
			LanePid = find_element(LaneId, LanePidList),
			case Exist of
				false  -> [{Dir, [{LaneId, [{id, LanePid} | Data]}]} | Res];
				_Other -> {Dir, List} = Exist,
						  NewList = lists:append(List, [{LaneId, [{id, LanePid} | Data] }]),
						  lists:keyreplace(Dir, 1, Res, {Dir, NewList})
			end
		end,
		[],
		RestoredLanes
	), RestoredStandBy}.
	
records(Dir, Lanes, FlowDir) ->
	Targets = find_element(Dir, Lanes),
	%io:format("[SENSOR] Sensor Lanes to count for ~w ~w ~n~n",[FlowDir, Targets]),
	case lists:any(fun({_Lane, Data}) -> Rain = find_element(rain, Data), Rain == 1 end, Targets) of
		true -> IsRaining = 1;
		false -> IsRaining = 0
	end,
		
	Res = [{real_count, lists:foldl(fun({_Lane, Data}, Sum) -> 
		Counter = find_element(FlowDir, Data),
		Sum + Counter
		end, 0, Targets)}, {rain, IsRaining}],
	%io:format("[SENSOR] Sensor records ~w~n",[Res]),
	Res.
	
records_for_sibling(MainDir, Lanes) ->
	TempData = records_for_sibling(MainDir, Lanes, []),
	io:format("[SENSOR] Records Data: ~w of Lanes: ~w ~n~n",[TempData, Lanes]),
	FinalData = 
		lists:foldl(fun({Key, Value}, Acc) ->
					{{real_count, RVal}, {rain, Val}} = Acc,
					NewAcc =
						case Key of
							real_count -> {{real_count, RVal + Value}, {rain, Val}};
							rain	  when Val == 1 -> {{real_count, RVal}, {rain, 1}};
							_Other -> Acc
						end,
					NewAcc
					end,
					{{real_count, 0},{rain, 0}},
					TempData),
	
	io:format("[SENSOR] Records FINAL Data: ~w~n",[FinalData]),				
	FinalData.

records_for_sibling(MainDir, [], []) ->
	[];
records_for_sibling(_MainDir, [], Data) ->
	Data;
records_for_sibling(MainDir, [{MainDir, Lanes} | Tail], Data) ->
	Res = records_for_sibling_aux(dsp_str, Lanes),
	%io:format("[SENSOR] [SENSOR] Result of count for dsp_str ~w~n",[Res]),	
	RainScoutering = lists:any(fun({_Lane, LaneData}) -> Rain = find_element(rain, LaneData), Rain == 1 end, Lanes),
	%io:format("[SENSOR] Rain Scoutering ~w ~n",[RainScoutering]),
	case RainScoutering  of
		true -> IsRaining = 1;
		false -> IsRaining = 0
	end,
	FinalData = [{rain, IsRaining} | Data],
	records_for_sibling(MainDir, Tail, [Res | FinalData]);
records_for_sibling(MainDir, [{Dir, Lanes} | Tail], Data) ->
	Res = records_for_sibling_aux(dsp_trn, Lanes),
	%io:format("[SENSOR] Result of count for dsp_trn ~w~n",[Res]),
	records_for_sibling(MainDir, Tail, [Res | Data]).
	
records_for_sibling_aux(FlowDir, Lanes) ->
	Res = {real_count, lists:foldl(fun({Lane, Data}, Sum) ->
		%io:format("[SENSOR] Calculating real_count with Data: ~w Current Sum = ~w ~n~n", [{Lane, Data} ,Sum]),
		Counter = find_element(FlowDir, Data),
		%io:format("[SENSOR] Getting counter for flowdir: ~w Result = ~w ~n~n", [FlowDir ,Counter]),
		Sum + Counter
		end, 0, Lanes)},
	Res.

%%Check if data exists and update it or add the new value	
update_siblings_scouting(CurrentData, NewData) ->
	lists:map(fun({Key, Value}) ->
		CurrentValue = find_element(Key, CurrentData),
		if CurrentValue =:= [] ->
			{Key, Value};
		   true ->
		   	update_siblings_scouting_aux(CurrentValue, {Key, Value})
		end
	  end,
	  NewData
	).
	
update_siblings_scouting_aux(CurrentValue, {rain, Value}) when Value == 1 ->
	{rain, Value};
update_siblings_scouting_aux(CurrentValue, {Key, Value}) ->
	{Key, Value + CurrentValue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_records(SensorPid, Dir) -> 
%%TODO Cambiar esto para que retorne ambos datos dsp_str y dsp_trn o la suma, ver como afecta el cambio de real_count
%%TODO si se pide para av sacar el count de los dsp_str y sumar el count de los dsp_trn de la otra dir y sumarlo en el real_count
%%NOTA: 02/06/2014 hacer un método aparte que haga la suma correspondiente
	
	SensorPid ! {records, self(), Dir, dsp_str},
	receive
		{reply_records, Data} -> {reply_records, Data};
		Other		  -> io:format("[SENSOR] Error returning records for sensor ~w~n", [Other]),
						{reply, error}
	end.

get_records_for_sibling(SensorPid, SourcesDirList) -> 
	SensorPid ! {records_for_sibling, self(), SourcesDirList},
	receive
		{reply_sensor_records, Data} -> {reply_records, Data};
		Other		-> io:format("[SENSOR] Error returning records for sensor ~w~n", [Other]),
						{reply, error}
	end.
	
car_pass(SensorPid, LaneId, Count, Dir) ->
	%io:format("[SENSOR] ~nSensor call to exec ~w for lane: ~w with data: ~w~n", [SensorPid, LaneId, {Dir, Count}]),	
	SensorPid ! {update, self(), LaneId, Count, Dir},
	{ok, sensor}.
	
change(SensorPid, Dir) ->
	SensorPid ! {change, self(), Dir},
	{ok, sensor}.

idle(SensorPid, Dir) ->
	SensorPid ! {idle, self(), Dir},
	{ok, sensor}.
	
standby(SensorPid) ->
	SensorPid ! {standby, self()},
	{ok, sensor}.
	
check_standby(SensorPid, Limit) ->
	SensorPid ! {check_standby, self(),Limit},
	receive
		{reply, Status} -> {reply, Status};
		_Other			-> {reply, error}
	end.	
	
