-module(sensor).

-export([start/1, get_records/2, car_pass/4, standby/1, change/2, idle/2, check_standby/2]).

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
			io:format("Sensor msg ~w for lane: ~w with data: ~w~n", [self(), CallerId, {Dir, Value}]),
			NewLanes = update_count(CallerId, Lanes, Lanes, Value, Dir),			
			sensor(NewLanes, File, 0);
		{standby, _CallerPid} ->
			sensor(Lanes, File, StandBy + 1);
		{change, _CallerPid, Dir} ->
			io:format("Change Sensor targets ~n", []),
			NewLanes = reset_count(Lanes, Dir,0),			
			sensor(NewLanes, File, StandBy);
		{idle, _CallerPid, Dir} ->
			io:format("Idle Sensor targets ~n", []),
			NewLanes = reset_count(Lanes, Dir,-1),
			io:format("Sensor new lanes count ~w~n",[NewLanes]),
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
			reply(CallerPid, Data),
			sensor(Lanes, File, StandBy);
		{stop, _CallerPid} ->
			%reply(CallerPid, ok),
			{normal, sensor}
	end.
	
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.
    
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
	io:format("CHECKPOINT SENSOR file : ~p ~n",[File]),
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
	%io:format("SensorFileName to restore: ~p, lanes: ~w~n~n", [File, Lanes]),
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
	case lists:any(fun({_Lane, Data}) -> Rain = find_element(rain, Data), Rain == 1 end, Targets) of
		true -> IsRaining = 1;
		false -> IsRaining = 0
	end,
		
	[{real_count, lists:foldl(fun({_Lane, Data}, Sum) -> 
		Counter = find_element(FlowDir, Data),
		Sum + Counter
		end, 0, Targets)}, {rain, IsRaining}].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_records(SensorPid, Dir) -> 
	SensorPid ! {records, self(), Dir, dsp_str},
	receive
		{reply, Data} -> {reply, Data};
		_Other		  -> {reply, error}
	end.

car_pass(SensorPid, LaneId, Count, Dir) ->
	%io:format("~nSensor call to exec ~w for lane: ~w with data: ~w~n", [SensorPid, LaneId, {Dir, Count}]),	
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
	
