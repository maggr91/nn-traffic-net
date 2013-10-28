-module(sensor).

-export([start/1, get_records/2]).

-export([init/1]).


start(Args) ->
	spawn(sensor, init, [Args]).
	
init({normal, Args}) ->
	{Lanes, File} = Args,
	NewLanes = format_lanes(Lanes),
	%%NewFile = formated_log(File),
	sensor(NewLanes, File);

init({restore, Args}) ->
	{Lanes, File} = Args,
	RestoredLanes = restore(Lanes, File),
	sensor(RestoredLanes, File).

sensor(Lanes, File) ->
	receive 
		{update, _CallerPid, CallerId} ->
			NewLanes = update_count(CallerId, Lanes, Lanes),
			sensor(NewLanes, File);
		{checkpoint, _CallerPid} ->
			filelib:ensure_dir("checkpoint/sensors/"),
			write_checkpoint(Lanes, File),
			sensor(Lanes, File);
		{update_file, NewFile} ->
			sensor(Lanes, NewFile);
		{records, CallerPid, Dir} ->
			Data = records(Dir, Lanes),
			reply(CallerPid, Data),
			sensor(Lanes, File);
		{stop, CallerPid} ->
			reply(CallerPid, ok),
			{normal, sensor}
	end.
	
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.
    
format_lanes(Lanes) ->
	lists:map(fun({Dir, List}) ->
		{Dir, lists:map( fun({LaneId, LanePid}) -> {LaneId, [{id, LanePid}, {round_count, 0}, {total, 0}]} end, List)}
		end,
		Lanes
	).

update_count(_LaneId, [], OldLanes) ->
	OldLanes;
update_count(LaneId, [{Dir, LanesList} | Lanes], OldLanes) ->
	Exist = lists:keyfind(LaneId, 1, LanesList),
	case Exist of
		false  -> update_count(LaneId, Lanes, OldLanes);
		_Other -> {LaneId, Data} = Exist,
				  NewData = update_data_count([{round_count, 1}, {total, 1}], Data),
				  NewLaneList = lists:keyreplace(LaneId,1, LanesList, {LaneId, NewData}),
				  lists:keyreplace(Dir,1, OldLanes, {Dir, NewLaneList})
	end.

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
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checkpoint
write_checkpoint(Lanes, File)->
	io:format("CHECKPOINT SENSOR file : ~p ~n",[File]),
	FormatedLanes = format_for_check(Lanes),
	lists:map(fun(Data) -> 
		filemanager:write_raw(File, io_lib:format("~w", [Data]))
		end,
		FormatedLanes),
	{normal, checkpoint}.

format_for_check(Lanes) ->
	lists:append(
		lists:map(
			fun({Dir, List}) -> 
				lists:map(
					fun({LaneId, Data})-> 
						CheckData = lists:filter(fun({Key, _Value}) -> Key =/= id end, Data), 
						{Dir, LaneId, CheckData} 
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
	lists:foldl(
		fun({Dir,LaneId,Data}, Res) ->
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
	).
	
records(Dir, Lanes) ->
	Targets = find_element(Dir, Lanes),
	lists:foldl(fun({_Lane, Data}, Sum) -> 
		Counter = find_element(round_count, Data),
		Sum + Counter
		end, 0, Targets).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_records(SensorPid, Dir) -> 
	SensorPid ! {},
	Data = Dir,
	Data.

