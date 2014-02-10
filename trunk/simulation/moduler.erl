-module(moduler).
-export([start/0,start/1, test/0, connect/3,status/1, status_test/0, update_from_nn/3, request_updates/2, 
	checkpoint/1, stop/1, get_sensor/1, reset_sensor/2, estimation_proc/3, check_sensor_standby/2, change_mode/2]).
-export([init/1, update_dm/3, delete_old/1]).

%% Used to comunicate traffic lights 
%% this acts like a buffer for the communication
%% stores a default value first, so if it's called by any other traffic light
%% it will answer with the value that it already has this to avoid extra
%% load to the lights, an so they focus to use the network an simulate

start() ->
	spawn(moduler, init, [dummyLight]).

start(Args) ->
	spawn(moduler, init, [Args]).

init({restore, LightId, Lanes}) ->
	[Config | _Junk] = get_config(),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	FixedCarLength = find_config_data(Config, fixed_car_length),
	Mode = find_config_data(Config, mode),
	
	{ModFile, NNFile, SensorFile} = CheckpointLog,
	
	FormatNNFile = format_dm_file(NNFile, LightId),
	{_Struct, Mapping, {_, SubMappingLength}} = find_dm_config_data(Config, LightId),
	NN = restore_dm(FormatNNFile, Mapping),
	
	RestoredData = restore(ModFile, LightId),
	
	Sensor = restore_sens(SensorFile,Lanes, LightId),
	%timer:apply_after(100, moduler, update_dm, [NN, NNFile, LightId]),
	
	%format log files
	FormatLog = formated_log(ModFile),
	FormatSens = format_dm_file(SensorFile, LightId),
	
	%delete_old([FormatLog, FormatSens]),
	timer:apply_after(100, moduler,delete_old,[[FormatLog, FormatSens]]),
	
	NewCheckpointLog = {FormatLog, FormatNNFile, FormatSens},
	Trainer = trainer_analyzer:start(),
	
	listen(LightId,[{av, []}, {ca,[]}], NN,RestoredData,NewCheckpointLog, Sensor, 
		{Trainer, Mode}, FixedCarLength, SubMappingLength);

init({normal, LightId, Lanes}) ->
	[Config | _Junk] = get_config(),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	FixedCarLength = find_config_data(Config, fixed_car_length),
	Mode = find_config_data(Config, mode),
	{Other, NNFile, SensorFile} = CheckpointLog,
	
	FormatLog = formated_log(Other),
	FormatSens = format_dm_file(SensorFile, LightId),
	FormatNNFile = format_dm_file(NNFile, LightId),
	
	delete_old([FormatLog, FormatSens]),
	
	NewCheckpointLog = {FormatLog, FormatNNFile, FormatSens},
	{NN, SubMappingLength} = create_dm(Config,FormatNNFile, LightId),
	%update_dm(NN, NNFile, LightId),	
	
	Sensor = create_sens(Lanes, FormatSens),
	Trainer = trainer_analyzer:start(),
	
	listen(LightId,[{av, []}, {ca,[]}], NN,[{siblings_data, []},{net_values, []}, {net_input, []}, {sensor_input, []}],
		NewCheckpointLog, Sensor, {Trainer, Mode}, FixedCarLength, SubMappingLength).
		
%%CREATES The decision maker for the light in this case is a Neuronal Network
%%Input: None
%%Output: Pid of the DM
create_dm(Config, NNFile, Light)->
	Exist = filelib:is_file(NNFile),
	NNConfig = find_dm_config_data(Config, Light),
	{{Input, Hidden, Output}, Mapping, {_, SubMappingLength}} = NNConfig,
	if	Exist =:= false ->
			io:format("~n~n MISSING LEARNING FILE creating new network~n~n"),
			%NNConfig = find_config_data(Config, nn_config),
			{ann:start(Input, Hidden, Output, NNFile, Mapping), SubMappingLength};
		true ->
			io:format("~n~nLEARNING FILE ENCOUNTER ~p  restoring network~n~n",[NNFile]),
			{restore_dm(NNFile, Mapping), SubMappingLength}
	end.

update_dm(NN, NNFile, LightId)->
	NewNNFile = format_dm_file(NNFile, LightId),
	NN ! {update_file, NewNNFile}.

restore_dm(NNFile, Mapping) ->
	%NewLight = atom_to_list(LightId) ++ ".txt",
	%NewNNFile = NNFile ++ NewLight,
	%NewNNFile = format_dm_file(NNFile, LightId),
	ann:start({NNFile, Mapping}).

%%%%%%%%%%%
%% SENSOR

create_sens(Lanes, FormatSens) ->
	sensor:start({normal, {Lanes, FormatSens}}).
	
restore_sens(SensorFile,Lanes, LightId) ->
	NewLight = atom_to_list(LightId) ++ ".txt",
	NewSensFile = SensorFile ++ NewLight,
	Pid = sensor:start({restore, {Lanes, NewSensFile}}),
	
	FinalSensFile = format_dm_file(SensorFile, LightId),
	io:format("Updating sensor file ~p", [FinalSensFile]),
	Pid ! {update_file, FinalSensFile},
	Pid.

format_dm_file(NNFile, LightId) ->
	{ok, Cwd} = file:get_cwd(),
	NewLight = atom_to_list(LightId) ++ ".txt",
	FileAux = Cwd ++ NNFile,
	FileAux ++ NewLight.
	
formated_log(File) ->
	{ok, Cwd} = file:get_cwd(),
	Cwd ++ File.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping) ->
	receive
		%%call siblings
		{broadcast_request, _CallerPid, Dir} -> %%Ask siblings to send an update
			io:format("Data before broadcast: ~w", [Data]),
			NewData = request_update_from_Siblings(Siblings, Data, Dir),
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{broadcast_update, CallerPid, NewData, Dir} -> %%Send siblings to an update
			io:format("Update triggered by siblings ~w Light: ~w... NewData", [CallerPid,Light]),
			UpdateData = update_siblings_data(CallerPid, Data, NewData, Dir),
			listen(Light, Siblings, NN, UpdateData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{response, CallerPid, Dir} ->
			%%%EVALUAR SI LO QUE RETORNA EL SENSOR ES ACTIVO O IDLE(-1 o numeros negativos) si es malo retornar
			%%% el que tenga almacenado el modulador, si no dar version reciente			 
			{reply, SensorInputs} = sensor:get_records(Sensor, Dir),
			%{SensorRespond, UpdatedData} = eval_sensor_input(SensorInputs, Data,Dir),
			{Return, UpdatedData} = eval_sensor_input(SensorInputs, Data, Dir),
					
			%Return = lists:append([ResponseData, SensorRespond]),
			io:format("~n~n Return data from ~w ~w~n", [CallerPid, Return]),	
			reply(CallerPid, Return),
			listen(Light, Siblings, NN, UpdatedData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
			%% send info to caller
		{update_nn, CallerPid, NewVals, Dir} ->
			NewData = update_nn_data(net_values, Data, NewVals,Dir),
			reply(CallerPid, ok),
			send_update_to_Siblings(Light, Siblings, NewVals, Dir),
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{inputs, _CallerPid, NewVals} ->
			NewData = update_nn_data(net_input, Data, NewVals),			
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{connect, SiblingData, Dir} ->
			%%add connection to siblings list but not the data
			%{Dir, List} = lists:keyfind(Dir, 1, Siblings),
			ExistDir = lists:keyfind(Dir, 1, Siblings),
			case ExistDir of
				false -> NewSiblings = [{Dir, []} | Siblings],
						 List = [];
				_Other  -> {Dir, List} = ExistDir,
						 NewSiblings = Siblings
			end,
			 
			Exist = lists:any(fun(Item) -> Item == SiblingData end, List),
			case Exist of
				false -> UpdatedSiblings = lists:keyreplace(Dir, 1, NewSiblings, {Dir, [SiblingData | List]}),
						 listen(Light, UpdatedSiblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
				true  -> listen(Light, NewSiblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping)
			end;
		{proc, CallerPid, Dir, Stats} ->			
			%%First, get data from related sensor(s)
			%%return is [{real_count, Val}, {rain, Val}]
									
			%%%%%%%%%%NEW
			%secure that Dir is a list
			DirList = lists:append([Dir]),
			
			%%First, get data from related sensorupdates(s)
			io:format("~n~nCALLING MANAGE INPUTS UPDATES ON PROCESS ~w ~n~n",[{Sensor, DirList, Data, Siblings}]),
			{Updates, NewData} = manage_inputs_updates(Sensor, DirList, Data, Siblings),
			
			io:format("~n~nCALLING FORMAT INPUTS ON PROCESS ~w ~n~n",[{Updates, NewData}]),
			%%Second, format all external data with info from the light
			FormatedInputs = format_inputs(DirList, NewData, Updates),
			%FormatedInputs = format_inputs(DirList, NewData,SensorInputs, SiblingsInput),
			
			%%EXECUTE
			FinalDesition = execute(DirList, NN, Stats, Trainer, FormatedInputs, NewData, FixedCarLength, Light, Mapping),
			%%Send the info back to the light
			reply(CallerPid, FinalDesition),
			
			FinalData = manage_data_updates(Light, Siblings, DirList, NewData, FinalDesition, Updates),

			%%updateValues
			%update_from_nn(self(), FinalDesition,DirList),
			
			%listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor);
			listen(Light, Siblings, NN, FinalData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
			
		{stop, _CallerPid} ->
			force_stop(NN, Sensor),
			%reply(CallerPid, {normal, moduler}),find_dm_config_data(Config, Target) ->			
			{normal, moduler};
		{checkpoint, CallerPid} ->
			write_checkpoint(NN, Data, CheckpointLog, Light, Sensor),
			reply(CallerPid, ok),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{active_sensor, CallerPid} ->
			reply(CallerPid, Sensor),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{change_sensor, _CallerPid, Dir} ->
			sensor:change(Sensor, Dir),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{check_sensor_standby, CallerPid, Limit} ->
			Res = sensor:check_standby(Sensor, Limit),
			reply(CallerPid, Res),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{status, _CallerPid} ->
			io:format("Status of moduler ~w ~n Siblings: ~w~n Data: ~w~n~n",[{Light,self()}, Siblings, Data]),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{update_mode, NewMode, CallerPid} ->
			{TrainerPid, _OldMode} = Trainer,
			reply(CallerPid, ok),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, {TrainerPid, NewMode}, FixedCarLength, Mapping)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     END  MAIN          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     GENERAL FUNCTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_desition(FormatedInputs,Dir, Light, NN, CarStats, {TrainerPid, train}) ->
	%%Call trainer prior calling the NN to get a waitedResult
	%%call nn with the new inputs
	Desition = ann:input_set(NN, {train, [{inputs, FormatedInputs}]}),
	io:format("GETTING TRAINER INFO", []),
	S = atom_to_list(Light),
    ParentLaneId = list_to_atom(string:concat(atom_to_list(Dir), string:substr(S,6,3))),
	trainer_analyzer:evaluate(TrainerPid, ParentLaneId, CarStats),	
	[10,3,1];
	

get_desition(FormatedInputs, _Dir, _Light, NN, _CarStats, {_TrainerPid, normal}) ->
	%%Call trainer prior calling the NN to get a waitedResult
	%%call nn with the new inputs
	Res = ann:input_set(NN, {normal, [{inputs, FormatedInputs}]}),
	case Res of
				{reply, Value} ->	
						  Value;			  
				Other -> {error, Other}
	end.


reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

%connect(Dir, {SenderId, SenderPid}, {ReceiverId, ReceiverPid}) ->
%	SenderPid ! {connect, {ReceiverId, ReceiverPid}, Dir},
%	ReceiverPid ! {connect, {SenderId, SenderPid}, Dir}.
connect(Dir, {SenderId, SenderPid, SenderLocation}, {ReceiverId, ReceiverPid, ReceiverLocation}) ->
	SenderPid ! {connect, {ReceiverId, ReceiverPid,ReceiverLocation}, Dir},
	ReceiverPid ! {connect, {SenderId, SenderPid,SenderLocation}, Dir}.
	
%connect(Dir, SenderPid, ReceiverPid) ->
%	SenderPid ! {connect, ReceiverPid, Dir}.

request_update_from_Siblings([], Data, _TargetDir) ->
	io:format("Data after broadcast: ~w~n", [Data]),
	Data;
request_update_from_Siblings([{Dir, SiblingsList} | Siblings], Data, Dir) ->
	io:format("Updating ~w List: ~w~n", [Dir, SiblingsList]),
	NewData = update_from_Siblings_aux(SiblingsList, Data, Dir),
	request_update_from_Siblings(Siblings, NewData, Dir);
request_update_from_Siblings([_OtherDir | Siblings], Data, TargetDir) ->	
	request_update_from_Siblings(Siblings, Data, TargetDir).
	
update_from_Siblings_aux([], Data, _Dir) ->
	Data;
update_from_Siblings_aux([{Sid, S, _SibLocation} | Siblings], Data, Dir) ->
	io:format("~nAsking for update from ~w~n~n",[Sid]),
	S ! {response, self(), Dir},
	receive
		{reply, NewData} ->
			io:format("Response of data update from ~w NewData ~w~n",[Sid, NewData]),
			UpdatedData = update_siblings_data(Sid, Data, NewData, Dir),
			update_from_Siblings_aux(Siblings, UpdatedData, Dir);
			%Res = lists:keyfind(S,1, Data),
			%case Res of
			%	false ->	update_from_Siblings_aux(Siblings, [{S, NewData} | Data]); 	
			%	_Other -> 	UpdatedData = lists:keyreplace(S,1,Data,{S, NewData}),
			%				update_from_Siblings_aux(Siblings, UpdatedData)
			%end;						
		_Other ->
			{error, badarg}			
	end.


send_update_to_Siblings(Id, Siblings, Data, TargetDir) ->
	lists:map(
		fun({Dir, SiblingsList}) ->
			io:format("Sending update to siblings on ~w. List: ~w~n~n", [Dir, SiblingsList]),
			lists:map(
				fun({_SiblingId, SiblingPid, _SiblingLocation}) ->
					SiblingPid ! {broadcast_update, Id, Data, TargetDir}
				end,
				SiblingsList
			)
		end,
		Siblings
	).								


update_siblings_data(Sibling, Data, NewData, Dir) ->
	{_Key, SiblingList} = lists:keyfind(siblings_data,1, Data),
	Item = lists:keyfind(Sibling,1, SiblingList),
	case Item of
		false ->
			io:format("No data founded, adding info for sibling"),
			NewSiblingData = [{Sibling,[{Dir, NewData}]} | SiblingList];
		{Sibling, Value} ->
			io:format("Previous data founded, updating for sibling"),			
			UpdatedData = safe_data_update(Dir, NewData, Value),
			NewSiblingData = lists:keyreplace(Sibling, 1, SiblingList, {Sibling, UpdatedData})
	end,
	lists:keyreplace(siblings_data, 1, Data, {siblings_data, NewSiblingData}).

update_nn_data(Id, Data, NewVals) ->
	%{net_values, _Values} = lists:keyfind(net_values, 1, Data),
	%lists:keyreplace(net_values, 1, Data, {net_values, NewVals}).	
	lists:keyreplace(Id, 1, Data, {Id, NewVals}).
	
update_nn_data(Id, Data, NewVals, Dir) ->   
	io:format("target ~w, UPDATE DATA NN ~w ... NewVal = ~w~n",[Id, Data, NewVals]),
	{Id, Values} = lists:keyfind(Id, 1, Data),
	DirData = lists:keyfind(Dir, 1, Values),
	case DirData of
		false 	   -> io:format("No record for dir ~w, UPDATE DATA with ~w~n",[Dir, {Dir, NewVals}]),					  
					  lists:keyreplace(Id, 1, Data, {Id, [{Dir, NewVals} | Values]});
		_Other 	   -> NewValues = lists:keyreplace(Dir, 1, Values, {Dir, NewVals}),
					  lists:keyreplace(Id, 1, Data, {Id, NewValues})
	end.


%%find configuration
find_config_data(ConfigData, Key) ->
	Res = lists:keyfind(Key, 1, ConfigData),
	case Res of
		false -> [];
		{Key, Value} -> Value
	end.

get_config() ->
	filemanager:get_data("/config_mod.txt").

%calculate_outputs(NetValues, NetInputs, FixedCarLength, Dir) ->
%%calculate_outputs(NetValues, _NetInputs, SensorInputs) ->
%	TargetInputs = find_element(Dir, NetInputs),
%	PromCars = calculate_promcars(FixedCarLength, TargetInputs, lists:nth(1,NetValues)),
	%PromCars = random:uniform(5),
%	ExtraOutputs = [{estimate_count, PromCars}],
%	NewValues = lists:append([NetValues, ExtraOutputs]),
%	reformat_net_values([cycle_time,umbral, delay],NewValues,[]).

calculate_outputs(NetValues, NetInputs, FixedCarLength, DirList) ->
	io:format("NetValues ~w NetInputs ~w FixedCarLength ~w Dirlist ~w ~n~n", [NetValues, NetInputs, FixedCarLength, DirList]),
	lists:map(fun(Dir) ->
			io:format("NetValues ~w NetInputs ~w FixedCarLength ~w Dir ~w ~n~n", [NetValues, NetInputs, FixedCarLength, Dir]),
			DirValues = find_element(Dir, NetValues),
			TargetInputs = find_element(Dir, NetInputs),
			io:format("TargetInputs ~w ~n~n", [TargetInputs]),
			PromCars = calculate_promcars(FixedCarLength, TargetInputs, lists:nth(1,DirValues)),
			%ExtraOutputs = [{estimate_count, PromCars}],
			ExtraOutputs = [{estimate_count, PromCars}],
			NewValues = lists:append([DirValues, ExtraOutputs]),
			%reformat all values to a key, value pair
			Final = reformat_net_values([cycle_time,umbral],NewValues,[]),
			{Dir, Final}
			end,
			DirList).

calculate_promcars(FixedCarLength, NetInputs, CycleTime) ->
	io:format("Calculating promcars ~w~n", [{FixedCarLength, CycleTime}]),
	%BCapacity = find_element(capacity, NetInputs),
	
	io:format("Looking for top_speed on ~w, ~n~n", [NetInputs]),
	{BTopSpeed, _BTopMove} = find_element(top_speed, NetInputs),
	io:format("TopSpeed~w, ~n~n", [BTopSpeed]),
	XmtsTop = (BTopSpeed * 1000 * CycleTime) / 3600,
	XmtsHalf = XmtsTop / 2,
	
	io:format("~nUn carro va a recorrer max unos ~w metros y min ~w aprox en ~w, ~n~n", [XmtsTop, XmtsHalf, CycleTime]),
	CarsTop = XmtsTop / FixedCarLength,%%calcular carros a pasar desde cada extremo
	CarsHalf = XmtsHalf / FixedCarLength,%%calcular carros a pasar desde cada extremo
	%random:uniform(5)
	CarsTop + CarsHalf / 2 .

reformat_net_values([], [], Reformated) ->
	lists:reverse(Reformated);
reformat_net_values([_Value | _Values], [], Reformated) ->
	lists:reverse(Reformated);
reformat_net_values([], [Value | Values], Reformated) ->
	reformat_net_values([], Values, [ Value | Reformated]);
reformat_net_values([Key | Keys], [Value | Values], Reformated) ->
	reformat_net_values(Keys, Values, [ {Key, Value} | Reformated]).


format_inputs(DirList, Data, Updates) ->
	NetInputs = find_element(net_input, Data),
	
	Temp = lists:reverse(
	 lists:foldl(
		fun(Dir, R) -> 
			LoadedInputs = find_element(Dir, NetInputs),
			DirValues = find_element(Dir, Updates),
			
			SensorInputs = find_element(sensor, DirValues),
			SiblingsData = find_element(siblings, DirValues),
			
			io:format("List to append LoadInputs ~w, SensorInp: ~w , SiblingData ~w ~n",[LoadedInputs, SensorInputs, SiblingsData]),
			Merged = lists:append([LoadedInputs, SensorInputs, SiblingsData]),
			InputsValues = lists:map(fun({Key, Value}) -> if Key == top_speed -> {TopSpeed, _} = Value, TopSpeed; true -> Value end end, Merged),
			[InputsValues | R]
		end,
		[],
		DirList
	)),
	
	FinalInputs = lists:append(Temp),
	io:format("Inputs Temp ~w Inputs Final ~w~n",[Temp, FinalInputs]),
	convert_inputs(FinalInputs).

%format_inputs(Dir, Data, SensorInputs, SiblingsData) ->
%	NetInputs = find_element(net_input, Data),
%	LoadedInputs = find_element(Dir, NetInputs),
%	
%	io:format("List to append LoadInputs ~w, SensorInp: ~w , SiblingData ~w ~n",[LoadedInputs, SensorInputs, SiblingsData]),
%	Merged = lists:append([LoadedInputs, SensorInputs, SiblingsData]),
%	io:format("Merged List ~w ~n",[Merged]),
%	%lists:map(fun({Key, Value}) -> if Key == top_speed -> {TopSpeed, _} = Value, TopSpeed; true -> Value end end, Merged).
%	InputsValues = lists:map(fun({Key, Value}) -> if Key == top_speed -> {TopSpeed, _} = Value, TopSpeed; true -> Value end end, Merged),
%	convert_inputs(InputsValues).

format_siblings_inputs(Dir, Siblings, Data) ->
	TargetSiblings = find_element(Dir, Siblings),
	ElementsData = find_element(siblings_data,Data),
	io:format("Target siblings: ~w  , ElementsData : ~w~n",[TargetSiblings, ElementsData]),
	%%Get the info about sibling before this one
	FilteredSiblings = lists:filter(fun({_SiblingId, _SiblingPid, Location}) -> Location == antes end, TargetSiblings),
	
	io:format("FilteredSiblings: ~w~n",[FilteredSiblings]),
	FilteredItems = filter_elements([real_count, estimate_count], ElementsData, FilteredSiblings),
	
	Diff = lists:map(fun({SiblingId, _SiblingPid, _SiblingIdLocation}) ->
		Element = find_element(SiblingId, ElementsData),
		CarCount = find_element(real_count, Element),
		Estimate = find_element(estimate_count, Element),
		
		io:format("Element: ~w~n CarCount: ~w~n Estimate: ~w~n",[Element,CarCount,Estimate]),
		if CarCount =/= [], Estimate =/= [] -> CarCount - Estimate;
			true  -> 0
			
		end
		end,
		TargetSiblings
	),
	io:format("Sibling input to merge ~w~n",[FilteredItems]),
	CaseAnomally = lists:any(fun(Value) -> Value < 0 end, Diff),
	Anomally = if  CaseAnomally == false ->	0;
					true -> 1
			   end,
	lists:append([{anomally, Anomally}] , FilteredItems).
	
	%%TODO: recorrer lista Elements y sacar un estimado de si los carros de los
	%% semaforos vecinos se estan atascando o no -> carros a pasar - real y pasarlo como parametro a la red
	%% sacar la info al semaforo anterior.
	%%%%%FOR EACH ELEMENT IN THE LIST GET AND FORMAT THE DATA FROM DATALIST


filter_elements(Filters, ElementsData, FilteredSiblings) ->
	lists:append(lists:map(fun({SiblingId, _SiblingPid, _SiblingIdLocation}) ->
		io:format("Looking data for ~w in ~w~n",[SiblingId, ElementsData]),
		Data = find_element(SiblingId, ElementsData),
		io:format("FIlters: ~w   Data found data for ~w~n",[Filters, Data]),
		filter_elements_aux(Filters, Data, [])
		end,
		FilteredSiblings
	)).
	
filter_elements_aux([], _Data, Result) ->
	Result;
filter_elements_aux([Item | List], Data, Result) ->
	Info = lists:keyfind(Item,1, Data),
	case Info of
		%false ->	filter_elements_aux(List, Data, [{Item, -1} | Result]);
		false ->	filter_elements_aux(List, Data, [{Item, 0} | Result]);
		_Other ->	filter_elements_aux(List, Data, [{Item, Info} | Result])
	end.


eval_sensor_input(SensorInputs, CurrentData, Dir) ->	
	io:format("Eval sensorinputs ~n~n SensorInputs ~w~n CurrentData: ~w~n",[SensorInputs, CurrentData]),
	SensorVal = find_element(real_count, SensorInputs),
	Rain = find_element(rain, SensorInputs),
		
	SensorInput = find_element(sensor_input, CurrentData),	
	
	DirNetValues = get_safe_net_values(CurrentData, Dir),
	TempDirCache = get_safe_sensor_inputs(Dir, SensorInput),
	
	io:format("~n~nSAFE sensor input on ~w with ~w~n~n", [Dir, TempDirCache]),
	CacheVals = lists:keyreplace(rain, 1, TempDirCache, {rain, Rain}),
	
	if 
		SensorVal < 0 ->			
			ResponseVals = lists:append([DirNetValues, CacheVals]),
			%UpdatedSensor = lists:keyreplace(Dir, 1, SensorInput, {Dir, CacheVals}),			
			%UpdatedData = lists:keyreplace(sensor_input, 1, CurrentData, {sensor_input, UpdatedSensor}),
			UpdatedData = update_nn_data(sensor_input, CurrentData, CacheVals, Dir),
			io:format("ResponseVals when sensor -1: ~w~n~n",[ResponseVals]),
			{ResponseVals,  UpdatedData};
		true		 ->
			NewSensorData = lists:keyreplace(real_count,1, CacheVals, {real_count, SensorVal}),			 
			%UpdatedSensor = lists:keyreplace(Dir, 1, SensorInput, {Dir, NewSensorData}),
			%UpdatedData = lists:keyreplace(sensor_input, 1, CurrentData, {sensor_input, UpdatedSensor}),
			UpdatedData = update_nn_data(sensor_input, CurrentData, NewSensorData, Dir),
			ResponseVals = lists:append([DirNetValues, NewSensorData]),
			io:format("ResponseVals when sensor is old: ~w~n~n",[ResponseVals]),
			{ResponseVals, UpdatedData}
	end.

%eval_sensor_input(SensorInputs, CurrentData, Dir) ->	
%	io:format("Eval sensorinputs ~n~n SensorInputs ~w~n CurrentData: ~w~n",[SensorInputs, CurrentData]),
%	SensorVal = find_element(real_count, SensorInputs),
%	Rain = find_element(rain, SensorInputs),
%	
%	io:format("Looking for sensor input on current data ~n"),
%	SensorInput = find_element(sensor_input, CurrentData),	
	%
%	DirNetValues = get_safe_net_values(CurrentData, Dir),
%	TempDirCache = find_element(Dir, SensorInput),
%	CacheVals = lists:keyreplace(rain, 1, TempDirCache, {rain, Rain}),
%	
%	if 
%		SensorVal < 0 ->			
%			ResponseVals = lists:append([DirNetValues, CacheVals]),
%			UpdatedSensor = lists:keyreplace(Dir, 1, SensorInput, {Dir, CacheVals}),
%			UpdatedData = lists:keyreplace(sensor_input, 1, CurrentData, {sensor_input, UpdatedSensor}),
%			{ResponseVals,  UpdatedData};
%		true		 ->
%			NewSensorData = lists:keyreplace(real_count,1, CacheVals, {real_count, SensorVal}),			 
%			UpdatedSensor = lists:keyreplace(Dir, 1, SensorInput, {Dir, NewSensorData}),
%			UpdatedData = lists:keyreplace(sensor_input, 1, CurrentData, {sensor_input, UpdatedSensor}),
%			ResponseVals = lists:append([DirNetValues, NewSensorData]),
%			{ResponseVals, UpdatedData}
%	end.

get_safe_net_values(CurrentData, Dir) ->
	NetValues = find_element(net_values, CurrentData),
	io:format("Looking for netval input of ~w on current data ~w ~n",[Dir, NetValues]),
	DirNetValues = find_element(Dir, NetValues),
	io:format("Get Net values for DIR ~w, Result: ~w",[Dir, DirNetValues]),
	if 
		DirNetValues == []; DirNetValues == false ->
			[{cycle_time,-1},{umbral,-1},{delay,-1},{estimate_count,1}];
		true ->
			DirNetValues
	end.
	
get_safe_sensor_inputs(_Dir, []) ->
	[{real_count, 0}, {rain, 0}];
get_safe_sensor_inputs(Dir, SensorData) ->
	TempDirCache = find_element(Dir, SensorData),
	case TempDirCache of
		[] 		->	[{real_count, 0}, {rain, 0}];
		_Other 	->	TempDirCache
	end.
	

%%INPUT: List -> list of numbers to convert
%%OUTPUT: List of numbers converted to binary (1 o 0)
%%DESC: Gets a sequence of numbers in a list, convert each one to binary and return a list in order 
%%		where a group of 1 and 0 represents the first number, the next group the second number and so on
convert_inputs(List) ->
	T = lists:reverse(lists:foldl(fun(E, Res) -> Bin = convert_to_binary(E), [Bin | Res] end, [], List)),
	lists:append(T).
		
convert_to_binary(Value) ->
	L = integer_to_list(Value, 2),
	lists:map(fun(Item) -> list_to_integer([Item]) end, L).
	
	
%%%MANAGE ALL INPUTS
manage_inputs_updates(Sensor, DirList, CurrentData, Siblings) ->
	lists:mapfoldl(fun(Dir, Data) -> 
				{reply, SensorInputs} = sensor:get_records(Sensor, Dir),
				%%sensor:idle(Sensor, Dir),		
				{_Return, FirstData} = eval_sensor_input(SensorInputs, Data, Dir),
				
				io:format("~n~n~n~n PROC DATA SENSOR UPDATE ~w~n~n~n~n",[FirstData]),
				
				%%second update data from siblings
				NewData = request_update_from_Siblings(Siblings, FirstData, Dir),
				
				%%Third format all reladted inputs from siblings				
				SiblingsInput = format_siblings_inputs(Dir, Siblings, NewData),
				io:format("SiblingsInput ~w",[SiblingsInput]),
				
				{{Dir, [{sensor, SensorInputs}, {siblings, SiblingsInput}]}, NewData}
								
			  end,
			  CurrentData,
			  DirList).
	
execute(DirList, NN, Stats, Trainer, FormatedInputs, CurrentData, FixedCarLength, Light, Mapping) ->
	Desition = get_desition(FormatedInputs,DirList, Light, NN, Stats, Trainer),
	io:format("Desition ~w~n",[{Desition, Mapping, DirList}]),
	DesitionFormated = split(Desition, Mapping, DirList),
	io:format("SPLITED VALUES ~w~n~n", [DesitionFormated]),
	NetInputs = find_element(net_input, CurrentData),
	%%Calculate extra outputs according to nn desition and the input data
	FinalDesition = calculate_outputs(DesitionFormated, NetInputs, FixedCarLength, DirList),
	%FinalDesition = calculate_outputs(Desition, NetInputs,SensorInputs),
			
	io:format("FinalDesition of NN: ~w~n",[FinalDesition]),
	FinalDesition.
			  
manage_data_updates(Light, Siblings, DirList, CurrentData, FinalDesition, Updates) ->
	lists:foldl(fun(Dir, NewData) -> 
				FinalData = update_nn_data(net_values, NewData, FinalDesition, Dir),
				DirValues = find_element(Dir, Updates),
				
				SensorInputs = find_element(sensor, DirValues),
				
				SiblingUpdateData = lists:append(FinalDesition, SensorInputs),
				io:format("Data after network update: FinalData .. ~w~n",[SiblingUpdateData]),
				
				send_update_to_Siblings(Light, Siblings, SiblingUpdateData, Dir),
				
				FinalData
								
			  end,
			  CurrentData,
			  DirList). 

split(List, Cs, KeyList) -> lists:reverse(split(List, Cs, KeyList, [])).
split([], _, _, Acc) -> Acc;
split(List, Cs, KeyList, Acc) ->
	{Chunk, Tail} = lists:split(Cs, List),
	[Key | KeyTail] = KeyList,
	split(Tail, Cs, KeyTail, [{Key, Chunk} | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%Update ann data
update_from_nn(ModulerPid, Values, Dir) ->
	ModulerPid ! {update_nn, self(), Values, Dir},
	receive
		{reply, ok} ->
			{ok, udpated};
		_Other ->
			io:format("error"),
			{error, []}
	end.
	
request_updates(ModulerPid, Dir) ->
	ModulerPid ! {broadcast_request, self(), Dir},
	{ok, updated}.

status(Pid) ->
	Pid ! {status, self()}.
	
status_test() ->
	L = [m1,m2,m3,m4,m5,m6,m7,m8,m9],
	lists:map(fun (Mod) -> Mod ! {status, self()} end, L),
	{ok, []}.

stop(ModulerPid) ->
	ModulerPid ! {stop, self()},
	{normal, moduler}.

%%INPUT: ModulerPid related to the traffic light, Dir direction where to look for info
%%OUTPUT: returns the new data to use by the light (mostly times)
%%DESC: Called from Lights, in order to ask the DM (nn) what would be the next cycle
estimation_proc(ModulerPid, Dir, Stats) ->
	ModulerPid ! {proc, self(), Dir, Stats},
	receive
		{reply, NewDesition} -> {reply, NewDesition};
		_Other ->	{error, moduler}		
	end.	

get_sensor(ModulerPid) ->
	ModulerPid ! {active_sensor, self()},
	receive
		{reply, Sensor} -> {reply, Sensor};
		_Other			-> {error, sensor}
	end.

reset_sensor(ModulerPid, Dir) ->
	ModulerPid ! {change_sensor, self(), Dir},
	{normal, moduler}.

check_sensor_standby(ModulerPid, Limit) ->
	ModulerPid ! {check_sensor_standby, self(), Limit},
	receive
		{reply, Status} -> {reply, Status};
		_Other		 	-> {reply, continue}
	end.
	
change_mode(ModulerPid, Mode) ->
 	ModulerPid ! {update_mode,self(), Mode},
 	receive
 		{reply, ok} -> ok;
 		Others -> Others
 	end.
 	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHECKPOINT
%%%%%%%%%%%%%
checkpoint(Pid) ->
	Pid ! {checkpoint, self()},
	receive
		{reply, ok} -> {reply, ok};
		_Other		-> {reply, error}
	end.
	

write_checkpoint(NN, Data, CheckpointLog, LightId, Sensor) ->
	FormatedSiblings = format_siblings(Data),
	FormatedData = lists:keyreplace(siblings_data,1,Data, {siblings_data, FormatedSiblings}),
	{ModulerFile, _NNFile, _SensorLight} = CheckpointLog,
	%io:format("moduler file: ~p", [ModulerFile]),
	filemanager:write_raw(ModulerFile, io_lib:format("~w", [{LightId, FormatedData}])),
	write_checkpoint_nn(NN),
	write_checkpoint_sens(Sensor).

write_checkpoint_nn(NN) ->
	NN ! {checkpoint, self()}.

write_checkpoint_sens(Sensor) ->
	Sensor ! {checkpoint, self()}.
	
format_siblings(Data) ->
	{_Key, SiblingList} = lists:keyfind(siblings_data,1, Data),
	lists:map(fun ({Sibling, NewData})-> {Sibling, NewData} end, SiblingList).
	

%%Uses the default file load in the Process to restore the data
restore(File, LightId) ->
	Data = filemanager:get_data(File),
	find_element(LightId, Data).

find_element(_Id, []) ->
	[];
find_element(Id, Data) ->
	Element = lists:keyfind(Id, 1, Data),
	case Element of
		false ->	[];
		_Other ->	{Id, Value} = Element,
					Value
	end.

force_stop(NN, Sensor) ->
	NN ! {stop, self()},
	Sensor ! {stop, self()}.

delete_old(List) ->
	lists:map(fun(File) -> file:delete(File) end, List). %% delete old log


%%INPUT: Key to look for
%%		 Data: New data to add or update
%%		 List: Container of the key value
%%OUTPUT: Updated List
%%%DESC: Update data for the key value, if its found replace it, if not add new data
safe_data_update(Key, Data, List) ->
	Element = lists:keyfind(Key, 1, List),
	case Element of
		false  -> [{Key, Data} | List];
		_Other -> lists:keyreplace(Key, 1, List, {Key, Data})
	end.


find_dm_config_data(Config, Target) ->
	NNConfigFile = find_config_data(Config, nn_config),
	DMConfigs = filemanager:get_data(NNConfigFile),
	Element = lists:keyfind(Target, 1, DMConfigs),
	case Element of
		false ->	{_Id, Value} = lists:keyfind(all, 1, DMConfigs),
					Value;
		_Other ->	{_Id, Value} = Element,
					Value
	end.

test() ->
	M1 = moduler:start(),
	M2 = moduler:start(),
	M3 = moduler:start(),
	M4 = moduler:start(),
	M5 = moduler:start(),
	M6 = moduler:start(),
	M7 = moduler:start(),
	M8 = moduler:start(),
	M9 = moduler:start(),
	
	
	register(m1,M1),
	register(m2,M2),
	register(m3,M3),
	register(m4,M4),
	register(m5,M5),
	register(m6,M6),
	register(m7,M7),
	register(m8,M8),
	register(m9,M9),
	
	moduler:connect(av, M1, M3),
	moduler:connect(av, M3, M5),
	moduler:connect(av, M5, M7),
	moduler:connect(av, M7, M9),
	
	moduler:connect(ca, M1, M2),
	moduler:connect(ca, M3, M4),
	moduler:connect(ca, M5, M6),
	moduler:connect(ca, M7, M8).
