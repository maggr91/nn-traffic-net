-module(moduler).
-export([start/0,start/1, connect/3,status/1, status_test/0, update_from_nn/3, request_updates/2, 
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
	Logs = find_config_data(Config, logs),
	FixedCarLength = find_config_data(Config, fixed_car_length),
	Mode = find_config_data(Config, mode),
	
	{NNLog} = Logs,
	{ModFile, NNFile, SensorFile} = CheckpointLog,
	
	FormatNNFile = format_dm_file(NNFile, LightId),
	FormatNNLog = format_dm_file(NNLog, LightId),
	{_Struct, Mapping, {_, SubMappingData}} = find_dm_config_data(Config, LightId),
	NN = restore_dm(FormatNNFile, Mapping, FormatNNLog),
	
	RestoredData = restore(ModFile, LightId),
	
	Sensor = restore_sens(SensorFile,Lanes, LightId),
	%timer:apply_after(100, moduler, update_dm, [NN, NNFile, LightId]),
	
	%format log files
	FormatLog = formated_log(ModFile),
	FormatSens = format_dm_file(SensorFile, LightId),
	
	%delete_old([FormatLog, FormatSens]),
	timer:apply_after(100, moduler,delete_old,[[FormatLog, FormatSens]]),
	
	NewCheckpointLog = {FormatLog, FormatNNFile, FormatSens, FormatNNLog},
	Trainer = trainer_analyzer:start(),
	
	listen(LightId,[{av, []}, {ca,[]}], NN,RestoredData,NewCheckpointLog, Sensor, 
		{Trainer, Mode}, FixedCarLength, SubMappingData);

init({normal, LightId, Lanes}) ->
	[Config | _Junk] = get_config(),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	Logs = find_config_data(Config, logs),
	
	FixedCarLength = find_config_data(Config, fixed_car_length),
	Mode = find_config_data(Config, mode),
	{Other, NNFile, SensorFile} = CheckpointLog,
	{NNLog} = Logs,
	
	FormatLog = formated_log(Other),
	FormatSens = format_dm_file(SensorFile, LightId),
	FormatNNFile = format_dm_file(NNFile, LightId),
	FormatNNLog = format_dm_file(NNLog, LightId),
	
	delete_old([FormatLog, FormatSens]),
	
	NewCheckpointLog = {FormatLog, FormatNNFile, FormatSens, FormatNNLog},
	{NN, SubMappingData} = create_dm(Config,FormatNNFile,FormatNNLog, LightId),
	%update_dm(NN, NNFile, LightId),	
	
	Sensor = create_sens(Lanes, FormatSens),
	Trainer = trainer_analyzer:start(),
	
	listen(LightId,[{av, []}, {ca,[]}], NN,[{siblings_data, []},{net_values, []}, {net_input, []}, {sensor_input, []}],
		NewCheckpointLog, Sensor, {Trainer, Mode}, FixedCarLength, SubMappingData).
		
%%CREATES The decision maker for the light in this case is a Neuronal Network
%%Input: None
%%Output: Pid of the DM
create_dm(Config, NNFile, Light)->
	Exist = filelib:is_file(NNFile),
	NNConfig = find_dm_config_data(Config, Light),
	{{Input, Hidden, Output}, Mapping, {_, SubMappingData}} = NNConfig,
	if	Exist =:= false ->
			io:format("[MODULER]  MISSING LEARNING FILE creating new network~n~n"),
			%NNConfig = find_config_data(Config, nn_config),
			{ann:start(Input, Hidden, Output, NNFile, Mapping), SubMappingData};
		true ->
			io:format("[MODULER] LEARNING FILE ENCOUNTER ~p  restoring network~n~n",[NNFile]),
			{restore_dm(NNFile, Mapping), SubMappingData}
	end.
	
%%CREATES The decision maker for the light in this case is a Neuronal Network
%%Input: None
%%Output: Pid of the DM
create_dm(Config, NNFile,NNLog, Light)->
	Exist = filelib:is_file(NNFile),
	NNConfig = find_dm_config_data(Config, Light),
	{{Input, Hidden, Output}, Mapping, {_, SubMappingData}} = NNConfig,
	if	Exist =:= false ->
			io:format("[MODULER]  MISSING LEARNING FILE creating new network~n~n"),
			%NNConfig = find_config_data(Config, nn_config),
			{ann:start(Input, Hidden, Output, NNFile, Mapping, NNLog), SubMappingData};
		true ->
			io:format("[MODULER] LEARNING FILE ENCOUNTER ~p  restoring network~n~n",[NNFile]),
			{restore_dm(NNFile, Mapping, NNLog), SubMappingData}
	end.

update_dm(NN, NNFile, LightId)->
	NewNNFile = format_dm_file(NNFile, LightId),
	NN ! {update_file, NewNNFile}.

restore_dm(NNFile, Mapping) ->
	ann:start({NNFile, Mapping}).
	
restore_dm(NNFile, Mapping, NNLog) ->
	ann:start({NNFile, Mapping, NNLog}).

%%%%%%%%%%%
%% SENSOR

create_sens(Lanes, FormatSens) ->
	sensor:start({normal, {Lanes, FormatSens}}).
	
restore_sens(SensorFile,Lanes, LightId) ->
	NewLight = atom_to_list(LightId) ++ ".txt",
	NewSensFile = SensorFile ++ NewLight,
	Pid = sensor:start({restore, {Lanes, NewSensFile}}),
	
	FinalSensFile = format_dm_file(SensorFile, LightId),
	io:format("[MODULER] Updating sensor file ~p", [FinalSensFile]),
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
			io:format("[MODULER] Data before broadcast: ~w", [Data]),
			NewData = request_update_from_Siblings(Siblings, Data, Dir),
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{broadcast_update, CallerPid, NewData, Dir} -> %%Send siblings to an update
			io:format("[MODULER] Update triggered by siblings ~w Light: ~w... NewData", [CallerPid,Light]),
			UpdateData = update_siblings_data(CallerPid, Data, NewData, Dir),
			listen(Light, Siblings, NN, UpdateData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		{response, CallerPid, Dir} ->
			%%%EVALUAR SI LO QUE RETORNA EL SENSOR ES ACTIVO O IDLE(-1 o numeros negativos) si es malo retornar
			%%% el que tenga almacenado el modulador, si no dar version reciente			 
			{reply_records, SensorInputs} = sensor:get_records_for_sibling(Sensor, Dir),
			io:format("[MODULER] Eval Sensor input on RESPONSE ~n"),
			{Return, UpdatedData} = eval_sensor_input(SensorInputs, Data, Dir),
					
			io:format("[MODULER]  Return data from ~w ~w~n", [CallerPid, Return]),	
			reply(reply_sibling_upd, CallerPid, Return),
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
			io:format("[MODULER] CALLING MANAGE INPUTS UPDATES ON PROCESS ~w ~n~n",[{Sensor, DirList, Data, Siblings}]),
			{Updates, NewData} = manage_inputs_updates(Sensor, DirList, Data, Siblings),
			
			io:format("[MODULER] CALLING FORMAT INPUTS ON PROCESS ~w ~n~n",[{Updates, NewData}]),
			%%Second, format all external data with info from the light
			FormatedInputs = format_inputs(DirList, NewData, Updates, Mapping),
			
			%%EXECUTE
			FinalDesition = execute(DirList, NN, Stats, Trainer, FormatedInputs, NewData, FixedCarLength, Light, Mapping, Siblings),
			%FormatDataResult = format_result_for_interface(Dir, FinalDesition, Mapping),
			%%Send the info back to the light
			reply(CallerPid, FinalDesition),
			
			FinalData = manage_data_updates(Light, Siblings, DirList, NewData, FinalDesition, Updates),

			%%updateValues
			%update_from_nn(self(), FinalDesition,DirList),
			
			%listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor);
			listen(Light, Siblings, NN, FinalData, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
		
		{performance, CallerPid, Dir, Stats} ->
			check_dm_performance(Trainer, NN, Dir, Stats, Light, [], []),
			reply(CallerPid, ok),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor, Trainer, FixedCarLength, Mapping);
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
			io:format("[MODULER] Status of moduler ~w ~n Siblings: ~w~n Data: ~w~n~n",[{Light,self()}, Siblings, Data]),
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

%%INPUTS: FormatedInputs: list of inputs to send to NN in order to get a result
%%		  DirList: list of the lanes to evaluate
%%		  Light: ID of the current Light
%%		  NN: PID of the NN
%%		  CarStats: list of cars with statistics
%%		  Mode : tuple with trainerPid and mode (train for selftraining, train_performance with helped training, and normal)
%%		  AnomalliesData: for each Dir in DirList indicates if theres a problem on lights located before (antes) current light
%%OUTPUTS: Desition: Decision taken by NN
%%DESC:   Get all data from current state an asked the NN for a decision
get_decision(FormatedInputs, _DirList, _Light, NN, _CarStats, {_TrainerPid, train}, _AnomalliesData, _OutputKeys) ->
	%%Call trainer prior calling the NN to get a waitedResult
	%%call nn with the new inputs
	get_dm_decision(NN, {train, [{inputs, FormatedInputs}]});
	
get_decision(FormatedInputs, DirList, Light, NN, CarStats, {TrainerPid, train_performance}, AnomalliesData, OutputKeys) ->
	%%Call trainer prior calling the NN to get a waitedResult
	%%call nn with the new inputs
	%%First do a evaluation of performance
	io:format("[MODULER] CHECKING TRAINER INFO~n~n", []),	
	check_dm_performance(TrainerPid, NN, DirList, CarStats, Light, AnomalliesData, OutputKeys),
	timer:sleep(100),
	io:format("[MODULER] AFTER CHECKING TRAINER INFO~n~n", []),	
	%%After that get a de
	get_dm_decision(NN, {normal, [{inputs, FormatedInputs}]});

get_decision(FormatedInputs, _DirList, _Light, NN, _CarStats, {_TrainerPid, normal}, _AnomalliesData, _OutputKeys) ->
	%%Call trainer prior calling the NN to get a waitedResult
	%%call nn with the new inputs
	get_dm_decision(NN, {normal, [{inputs, FormatedInputs}]}).


%%HELPER FUNCTION TO CALL NN WITH ARGS
get_dm_decision(NN, Args) ->
	io:format("[MODULER] GETTING TRAINER INFO", []),	
	Res = ann:input_set(NN, Args),
	case Res of
				{reply, Value} ->	
						  Value;			  
				Other -> {error, Other}
	end.

check_dm_performance(TrainerPid, NN, DirList, CarStats, Light, AnomalliesData, OutputKeys) ->
	ParentLaneIds = 
		lists:map(fun(Dir) ->
			S = atom_to_list(Light),
    		ParentLaneId = list_to_atom(string:concat(atom_to_list(Dir), string:substr(S,6,3))),
    		%ParentLaneId
    		{Dir, ParentLaneId}
    		end,
    		DirList),
    %%{OutputsMapping, NNLog}
    io:format("[MODULER] PARENTLANEIds ~w~n~n",[ParentLaneIds]),
    NNData = ann:get_data(NN),
    io:format("[MODULER] NNData ~w~n~n",[NNData]),
	Adjustment = trainer_analyzer:evaluate(TrainerPid, ParentLaneIds, CarStats, NNData, AnomalliesData, OutputKeys),
	case Adjustment of
		null	-> skip;
		_Other	-> ann:input_set(NN, {train, Adjustment})
	end,
	io:format("[MODULER] AFTER TRAINING ANN~n", []),	
	ok.



reply(Pid, Reply) ->
    Pid ! {reply, Reply}.
    
reply(ReplyKey, Pid, Reply) ->
    Pid ! {ReplyKey, Reply}.

connect(Dir, {SenderId, SenderPid, SenderLocation}, {ReceiverId, ReceiverPid, ReceiverLocation}) ->
	SenderPid ! {connect, {ReceiverId, ReceiverPid,ReceiverLocation}, Dir},
	ReceiverPid ! {connect, {SenderId, SenderPid,SenderLocation}, Dir}.

%%INPUT: Siblings list separated by direction,
%%		 Data: moduler current data
%%		 Dir: direction where the sibling is located
%%OUTPUT: Updated moduler data
%%DESC: Gets updates from ALL siblings on any direction and updates the data info
request_update_from_Siblings([], Data, _TargetDir) ->
	%io:format("[MODULER] Data after broadcast: ~w~n", [Data]),
	Data;
request_update_from_Siblings([{Dir, SiblingsList} | Siblings], Data, Dir) ->
	io:format("[MODULER] Updating ~w List: ~w~n", [Dir, SiblingsList]),
	NewData = update_from_Siblings_aux(SiblingsList, Data, Dir),
	request_update_from_Siblings(Siblings, NewData, Dir);
request_update_from_Siblings([_OtherDir | Siblings], Data, TargetDir) ->	
	request_update_from_Siblings(Siblings, Data, TargetDir).
	
update_from_Siblings_aux([], Data, _Dir) ->
	Data;
update_from_Siblings_aux([{Sid, S, _SibLocation} | Siblings], Data, Dir) ->
	%io:format("[MODULER] Asking for update from ~w~n~n",[Sid]),
	S ! {response, self(), Dir},
	receive
		{reply_sibling_upd, NewData} ->
			io:format("[MODULER] Response of data update from ~w NewData ~w~n",[Sid, NewData]),
			UpdatedData = update_siblings_data(Sid, Data, NewData, Dir),
			update_from_Siblings_aux(Siblings, UpdatedData, Dir);									
		Other		-> io:format("[MODULER] Error returning records for moduler ~w~n", [Other]),
						{reply, error}			
	end.


send_update_to_Siblings(Id, Siblings, Data, TargetDir) ->
	lists:map(
		fun({Dir, SiblingsList}) ->
			io:format("[MODULER] Sending update to siblings on ~w. List: ~w~n~n", [Dir, SiblingsList]),
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
	io:format("[MODULER] Updating sibling data for ~w with data: ~w on ~w dir ~w~n",[Sibling,Data, NewData, Dir]),
	{_Key, SiblingList} = lists:keyfind(siblings_data,1, Data),
	Item = lists:keyfind(Sibling,1, SiblingList),
	case Item of
		false ->
			io:format("[MODULER] No data founded, adding info for sibling ~n"),
			NewSiblingData = [{Sibling,[{Dir, NewData}]} | SiblingList];
		{Sibling, Value} ->
			io:format("[MODULER] Previous data founded, updating for sibling~n"),			
			UpdatedData = safe_data_update(Dir, NewData, Value),
			NewSiblingData = lists:keyreplace(Sibling, 1, SiblingList, {Sibling, UpdatedData})
	end,
	lists:keyreplace(siblings_data, 1, Data, {siblings_data, NewSiblingData}).

update_nn_data(Id, Data, NewVals) ->	
	lists:keyreplace(Id, 1, Data, {Id, NewVals}).
	
update_nn_data(Id, Data, NewVals, Dir) ->   
	%io:format("[MODULER] target ~w, UPDATE DATA NN ~w ... NewVal = ~w~n",[Id, Data, NewVals]),
	{Id, Values} = lists:keyfind(Id, 1, Data),
	DirData = lists:keyfind(Dir, 1, Values),
	case DirData of
		false 	   -> io:format("[MODULER] No record for dir ~w, UPDATE DATA with ~w~n",[Dir, {Dir, NewVals}]),					  
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
	filemanager:get_data("/config/config_mod.txt").

calculate_outputs(NetValues, NetInputs, FixedCarLength, DirList, OutputKeyMapping) ->
	[_Junk | OutputKeyMap] = OutputKeyMapping,
	io:format("[MODULER] NetValues ~w NetInputs ~w FixedCarLength ~w Dirlist ~w ~n~n", [NetValues, NetInputs, FixedCarLength, DirList]),
	lists:map(fun(Dir) ->
			io:format("[MODULER] NetValues ~w NetInputs ~w FixedCarLength ~w Dir ~w ~n~n", [NetValues, NetInputs, FixedCarLength, Dir]),
			DirValues = find_element(Dir, NetValues),
			TargetInputs = find_element(Dir, NetInputs),
			io:format("[MODULER] TargetInputs ~w ~n~n", [TargetInputs]),
			PromCars = calculate_promcars(FixedCarLength, TargetInputs, lists:nth(1,DirValues)),
			ExtraOutputs = [{estimate_count, PromCars}],
			NewValues = lists:append([DirValues, ExtraOutputs]),
			%reformat all values to a key, value pair
			io:format("[MODULER] Prereformat values ~w with Output keys ~w~n",[NewValues, OutputKeyMap]),
			Final = reformat_net_values(OutputKeyMap,NewValues,[]),
			io:format("[MODULER] Postreformat values ~w with Output keys ~w~n",[Final, OutputKeyMap]),
			%Final = reformat_net_values([cycle_time,umbral],NewValues,[]),
			{Dir, Final}
			end,
			DirList).

calculate_promcars(FixedCarLength, NetInputs, CycleTime) ->
	io:format("[MODULER] Calculating promcars ~w~n", [{FixedCarLength, CycleTime}]),
	%BCapacity = find_element(capacity, NetInputs),
	
	io:format("[MODULER] Looking for top_speed on ~w, ~n~n", [NetInputs]),
	{BTopSpeed, _BTopMove} = find_element(top_speed, NetInputs),
	io:format("[MODULER] TopSpeed~w, ~n~n", [BTopSpeed]),
	XmtsTop = (BTopSpeed * 1000 * CycleTime) / 3600,
	XmtsHalf = XmtsTop / 2,
	
	io:format("[MODULER] Un carro va a recorrer max unos ~w metros y min ~w aprox en ~w, ~n~n", [XmtsTop, XmtsHalf, CycleTime]),
	CarsTop = XmtsTop / FixedCarLength,%%calcular carros a pasar desde cada extremo
	CarsHalf = XmtsHalf / FixedCarLength,%%calcular carros a pasar desde cada extremo
	%random:uniform(5)
	trunc(CarsTop + CarsHalf / 2).

reformat_net_values([], [], Reformated) ->
	lists:reverse(Reformated);
reformat_net_values([_Key | _Values], [], Reformated) ->
	lists:reverse(Reformated);
reformat_net_values([], [Value | Values], Reformated) ->
	reformat_net_values([], Values, [ Value | Reformated]);
reformat_net_values([{Key, _Bits} | Keys], [Value | Values], Reformated) ->
	reformat_net_values(Keys, Values, [ {Key, Value} | Reformated]).


%%INPUTS: DirList - list of directions managed by the intersection (av, ca, avf)
%%		  Data - Data handled by the moduler
%%		  Updates - InputData to be sent to the ANN
%%OUTPUTS: InputList formated to be used by the ANN
%%DESC: This method takes 
format_inputs(DirList, Data, Updates, Mapping) ->
	NetInputs = find_element(net_input, Data),
	InputKeys = find_child_element([keys, input_keys], Mapping),
		
	FinalInputs = 
	 lists:map(
		fun(Dir) -> 
			LoadedInputs = find_element(Dir, NetInputs),
			DirValues = find_element(Dir, Updates),
			
			SensorInputs = find_element(sensor, DirValues),
			SiblingsData = find_element(siblings, DirValues),
			
			%%InputsValues
			custom_arrange(LoadedInputs, SensorInputs, SiblingsData, InputKeys, Data, Dir)			
		end,
		DirList
	),
		
	io:format("[MODULER] Inputs Final ~w~n",[FinalInputs]),
	lists:append(FinalInputs).
	%convert_inputs(FinalInputs).

custom_arrange(LoadedInputs, SensorInputs, SiblingsData, InputKeys, Data, Dir) ->
	io:format("[MODULER] List to append LoadInputs ~w, SensorInp: ~w , SiblingData ~w ~n",[LoadedInputs, SensorInputs, SiblingsData]),
	InputsValues = lists:map(fun({Key, KeyLength}) ->
		Value = custom_arrange_aux(Key, LoadedInputs, SensorInputs, SiblingsData, Dir, Data),
		convert_to_fixed_binary(Value, KeyLength)		
		end,
		InputKeys),
	
	lists:append(InputsValues).
	
%%For each key select the proper option to do something and position it in the right place inside
%% the list to be sent
%{capacity,8}, {effiency_percent, 1}, {obs, 1}, {exceeds_spdlimit, 1}, {has_turnlanes, 1}, {has_morethan1_turnlanes, 1}, {anomallies_bf4,1}, {has_rain, 1}]}
custom_arrange_aux(capacity, LoadedInputs, _SensorInputs, _SiblingsData, _Dir, _Data) ->
	find_element(capacity, LoadedInputs);
custom_arrange_aux(effiency_percent, _LoadedInputs, _SensorInputs, _SiblingsData, Dir, Data) ->
	calc_effiency(Dir, Data);
custom_arrange_aux(obs, LoadedInputs, _SensorInputs, _SiblingsData, _Dir, _Data) ->
	find_element(count_obs, LoadedInputs);
custom_arrange_aux(exceeds_spdlimit, LoadedInputs, _SensorInputs, _SiblingsData, _Dir, _Data) ->
	Value = find_element(top_speed, LoadedInputs),
	{TopSpeed, _} = Value,
	if TopSpeed > 40 ->
			1;
		true ->
			0
	end;
	
custom_arrange_aux(has_turnlanes, LoadedInputs, _SensorInputs, _SiblingsData, _Dir, _Data) ->
	Value = find_element(lanes_two_way, LoadedInputs),
	if 	Value > 0 ->
			1;
		true ->
			0
	end;
	
custom_arrange_aux(has_morethan1_turnlanes, LoadedInputs, _SensorInputs, _SiblingsData, _Dir, _Data) -> 
	Value = find_element(lanes_two_way, LoadedInputs),
	if 	Value > 1 ->
			1;
		true ->
			0
	end;

custom_arrange_aux(anomallies_bf4, _LoadedInputs, _SensorInputs, SiblingsData, _Dir, _Data) ->
	find_element(anomally, SiblingsData);

custom_arrange_aux(has_rain, _LoadedInputs, SensorInputs, _SiblingsData, _Dir, _Data) ->
	find_element(rain, SensorInputs);
	
custom_arrange_aux(Other, _LoadedInputs, _SensorInputs, SiblingsData, _Dir, _Data) ->
	find_element(Other, SiblingsData).

calc_load_capacity(Dir, SensorInputs, LoadedInputs) ->
	io:format("[MODULER] Calculating road load~w~n~n",[{Dir, SensorInputs, LoadedInputs}]),
	Capacity = find_element(capacity, LoadedInputs),
	true.
	%%TODO sacar la info de siblings data, donde indiquen la cantidad de carros que siguieron hacia esta ruta
	%%		filtrar el semáforo usando la ubicacion before
	%%		cambiar el sensor para que retorne cuantos fueron directo, y cuantos doblaron
	

calc_effiency(Dir, Data) -> 
	SensorInput = find_element (sensor_input, Data),
	NetValues = find_element (net_values, Data),
		
	Res = evaluate_effiency(Dir, SensorInput, NetValues),
	io:format("[MODULER] Calculating DIR EFFIENCY RES : ~w SensorInput :~w net_values: ~w~n~n",[Res, SensorInput, NetValues]),
	if Res < 90 ->
			0;
		true ->
			1
	end.

format_siblings_inputs(Dir, Siblings, Data) ->
	TargetSiblings = find_element(Dir, Siblings),
	ElementsData = find_element(siblings_data,Data),
	io:format("[MODULER] Siblinngs ~w Target siblings: ~w  , ElementsData : ~w~n",[Siblings, TargetSiblings, ElementsData]),
	%%Get the info about sibling before this one
	FilteredSiblings = lists:filter(fun({_SiblingId, _SiblingPid, Location}) -> Location == antes end, TargetSiblings),
	
	io:format("[MODULER] FilteredSiblings: ~w~n",[FilteredSiblings]),
	FilteredItems = filter_elements([real_count, estimate_count], ElementsData, FilteredSiblings),
	
	Diff = lists:map(fun({SiblingId, _SiblingPid, _SiblingIdLocation}) ->
		Element = find_element(SiblingId, ElementsData),
		CarCount = find_element(real_count, Element),
		Estimate = find_element(estimate_count, Element),
		
		io:format("[MODULER] Element: ~w~n CarCount: ~w~n Estimate: ~w~n",[Element,CarCount,Estimate]),
		if CarCount =/= [], Estimate =/= [] -> CarCount - Estimate;
			true  -> 0
			
		end
		end,
		TargetSiblings
	),
	io:format("[MODULER] Sibling input to merge ~w~n",[FilteredItems]),
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
		io:format("[MODULER] Looking data for ~w in ~w~n",[SiblingId, ElementsData]),
		Data = find_element(SiblingId, ElementsData),
		io:format("[MODULER] FIlters: ~w   Data found data for ~w~n",[Filters, Data]),
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
	io:format("[MODULER] Eval sensorinputs ~n~n SensorInputs ~w~n CurrentData: ~w~n",[SensorInputs, CurrentData]),
	SensorVal = find_element(real_count, SensorInputs),
	Rain = find_element(rain, SensorInputs),
		
	SensorInput = find_element(sensor_input, CurrentData),	
	
	DirNetValues = get_safe_net_values(CurrentData, Dir),
	TempDirCache = get_safe_sensor_inputs(Dir, SensorInput),
	
	io:format("[MODULER] SAFE sensor input on ~w with ~w~n~n", [Dir, TempDirCache]),
	CacheVals = lists:keyreplace(rain, 1, TempDirCache, {rain, Rain}),
	
	if 
		SensorVal < 0 ->			
			ResponseVals = lists:append([DirNetValues, CacheVals]),
			UpdatedData = update_nn_data(sensor_input, CurrentData, CacheVals, Dir),
			%io:format("[MODULER] ResponseVals when sensor -1: ~w~n~n",[ResponseVals]),
			{ResponseVals,  UpdatedData};
		true		 ->
			NewSensorData = lists:keyreplace(real_count,1, CacheVals, {real_count, SensorVal}),			 
			UpdatedData = update_nn_data(sensor_input, CurrentData, NewSensorData, Dir),
			ResponseVals = lists:append([DirNetValues, NewSensorData]),
			%io:format("[MODULER] ResponseVals when sensor is old: ~w~n~n",[ResponseVals]),
			{ResponseVals, UpdatedData}
	end.

%eval_sensor_input(SensorInputs, CurrentData, Dir) ->	
%	io:format("[MODULER] Eval sensorinputs ~n~n SensorInputs ~w~n CurrentData: ~w~n",[SensorInputs, CurrentData]),
%	SensorVal = find_element(real_count, SensorInputs),
%	Rain = find_element(rain, SensorInputs),
%	
%	io:format("[MODULER] Looking for sensor input on current data ~n"),
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
	%io:format("[MODULER] Looking for netval input of ~w on current data ~w ~n",[Dir, NetValues]),
	DirNetValues = find_element(Dir, NetValues),
	%io:format("[MODULER] Get Net values for DIR ~w, Result: ~w",[Dir, DirNetValues]),
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
	io:format("[MODULER] Converting List: ~w to binary ~n", [List]),
	T = lists:map(fun(E) -> Bin = convert_to_binary(E), Bin end, List),
	%T = lists:reverse(lists:foldl(fun(E, Res) -> Bin = convert_to_binary(E), [Bin | Res] end, [], List)),
	lists:append(T).
		
convert_to_binary(Value) ->
	L = integer_to_list(Value, 2),
	lists:map(fun(Item) -> list_to_integer([Item]) end, L).

convert_to_fixed_binary(Value, Length) ->
	BinaryValue = convert_to_binary(Value),
	ExtraPrefix = create_dummy_binary_list(Length - length(BinaryValue)),
	lists:append(ExtraPrefix, BinaryValue).

create_dummy_binary_list(N) ->
	create_dummy_binary_list_helper(N, []).

create_dummy_binary_list_helper(N, Acc) when N > 0 ->
	create_dummy_binary_list_helper(N - 1, [0 | Acc]);
create_dummy_binary_list_helper(_N, Acc) ->
	Acc.
	
%%%MANAGE ALL INPUTS
manage_inputs_updates(Sensor, DirList, CurrentData, Siblings) ->
	lists:mapfoldl(fun(Dir, Data) -> 
				%{reply_records, SensorInputs} = sensor:get_records(Sensor, Dir),
				{reply_records, SensorInputs} = sensor:get_records(Sensor, Dir),
				timer:sleep(100),
				%%sensor:idle(Sensor, Dir),
				io:format("[MODULER] Eval Sensor input on EXECUTE ~n"),	
				{_Return, FirstData} = eval_sensor_input(SensorInputs, Data, Dir),
				
				%io:format("[MODULER] ~n~n PROC DATA SENSOR UPDATE ~w~n~n~n~n",[FirstData]),
				
				%%second update data from siblings
				NewData = request_update_from_Siblings(Siblings, FirstData, Dir),
				
				%%Third format all reladted inputs from siblings				
				SiblingsInput = format_siblings_inputs(Dir, Siblings, NewData),
								
				io:format("[MODULER] SiblingsInput ~w",[SiblingsInput]),
				
				{{Dir, [{sensor, SensorInputs}, {siblings, SiblingsInput}]}, NewData}
								
			  end,
			  CurrentData,
			  DirList).
			  
%%INPUTS: DirList: list of the lanes to evaluate
%%		  NN: PID of the NN
%%		  Stats: list of cars with statistics
%%		  FormatedInputs: list of inputs to send to NN in order to get a result
%%		  CurrentData: data at current state to be updated
%%		  FixedCarLength: car length to do some estimations
%%		  Light: ID of the current Light
%%		  Mapping: map of the NN results output to translate into separete decimal numbers by each dir
%%OUTPUTS: FinalDesition: Decision taken by NN
%%DESC:   Get all data from current state an asked the NN for a decision
execute(DirList, NN, Stats, Trainer, FormatedInputs, CurrentData, FixedCarLength, Light, Mapping, Siblings) ->
	%%Get the length and OutputKeys of each dir (it reapetes the same)
	MappingDivLength = find_element(submap_length, Mapping),
	OutputKeys = find_child_element([keys, output_keys], Mapping),
	AnomalliesData = get_back_trouble_bit(DirList, CurrentData, Siblings),
	
	Desition = get_decision(FormatedInputs,DirList, Light, NN, Stats, Trainer, AnomalliesData, OutputKeys),		
	io:format("[MODULER] Desition ~w~n",[{Desition, MappingDivLength, DirList}]),
	DesitionFormated = split(Desition, MappingDivLength, DirList),
	io:format("[MODULER] SPLITED VALUES ~w~n~n", [DesitionFormated]),
	NetInputs = find_element(net_input, CurrentData),
	%%Calculate extra outputs according to nn decision and the input data
	FinalDesition = calculate_outputs(DesitionFormated, NetInputs, FixedCarLength, DirList, OutputKeys),
	%FinalDesition = calculate_outputs(DesitionFormated, NetInputs, FixedCarLength, DirList),
			
	io:format("[MODULER] FinalDesition of NN: ~w~n",[FinalDesition]),
	FinalDesition.
			  
manage_data_updates(Light, Siblings, DirList, CurrentData, FinalDesition, Updates) ->
	lists:foldl(fun(Dir, NewData) -> 
				FinalDesitionForDir = find_element(Dir, FinalDesition),
				FinalData = update_nn_data(net_values, NewData, FinalDesitionForDir, Dir),
				DirValues = find_element(Dir, Updates),
				
				SensorInputs = find_element(sensor, DirValues),
				
				SiblingUpdateData = lists:append(FinalDesitionForDir, SensorInputs),
				io:format("[MODULER] Data after network update: FinalData .. ~w~n",[SiblingUpdateData]),
				
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


%%INPUT: Dirlist: list of directions to evaluate
%%		 
%%DESC: for all dirs in Dirlist get the anomally field

%%%%NOTA VER COMO AGREGAR NETVALUES, Y SENSOR INPUTS PARA OBTENER DATOS QUE PERMITAN SACAR LA EFICIENCIA
%%%OJO

get_back_trouble_bit(DirList, Data, SiblingsList) ->
	io:format("[MODULER] DirList: ~w~n~n",[DirList]),
	SensorInput = find_element (sensor_input, Data),
	NetValues = find_element (net_values, Data),
	Res = lists:map(
		fun(Dir) ->
			TargetSiblings = find_element(Dir, SiblingsList),
			ElementsData = find_element(siblings_data,Data),
			
			FilteredSiblingsAntes = lists:filter(fun({_SiblingId, _SiblingPid, Location}) -> Location == antes end, TargetSiblings),
			FilteredSiblingsDespues = lists:filter(fun({_SiblingId, _SiblingPid, Location}) -> Location == despues end, TargetSiblings),
			
			%FilteredItemsAntes = filter_elements([real_count, estimate_count], ElementsData, FilteredSiblingsAntes),
			%FilteredItemsDespues = filter_elements([real_count, estimate_count], ElementsData, FilteredSiblingsDespues),
			
			ProcessedSiblingsAntes = {antes, evaluate_anomally_by_sibling(FilteredSiblingsAntes, ElementsData)},
			ProcessedSiblingsDespues = {despues, evaluate_anomally_by_sibling(FilteredSiblingsDespues, ElementsData)},
			
			
			%%EVALUATE EFFIENCY
			DirEffiency = {effiency, evaluate_effiency(Dir, SensorInput, NetValues)},
			{Dir, [ProcessedSiblingsAntes, ProcessedSiblingsDespues, DirEffiency]}
		end,
		DirList
	),
	io:format("[MODULER] get_back_trouble_bit result list: ~w~n~n",[Res]),
	Res.
	%%lists:map(fun(Dir) ->
	%%		DirValues = find_element(Dir, Data),
	%%		%TargetSiblings = find_element(Dir, SiblingsList),
	%%		IsAnyANomally = find_element(anomally, DirValues),
	%%		Anomally = 
	%%			case IsAnyANomally of
	%%				[] -> 0;
	%%				Other -> Other
	%%			end,
	%%		{Dir, Anomally}
	%%		end,
	%%		DirList).

	%%%%%%%%%%%%EVALUAR SI SE CUMPLE EL PORCENTAJE DE EFICIENCIA EFFIENCIE
	%%%%%%%%%%%%VER SI HAY ANOMALIAS ANTES O DESPUES
	%%%%%%%%%%%% LUEGO ENVIAR TODOS ESTOS DATOS JUNTOS AL TRAINER_ANALYZER EN EL METODO EXECUTE
	
%%
evaluate_anomally_by_sibling(TargetSiblings, ElementsData) ->
	
	lists:map(fun({SiblingId, _SiblingPid, _SiblingIdLocation}) ->
		Element = find_element(SiblingId, ElementsData),
		CarCount = find_element(real_count, Element),
		Estimate = find_element(estimate_count, Element),
		
		io:format("[MODULER] Element: ~w~n CarCount: ~w~n Estimate: ~w~n",[Element,CarCount,Estimate]),
		Diff = if CarCount =/= [], Estimate =/= [] -> CarCount - Estimate;
			true  -> 0
			
		end,
		
		CaseAnomally = Diff < 0,
		Anomally = if  CaseAnomally == false ->	0;
					true -> 1
			   end,
		{SiblingId, Anomally}
		end,
		TargetSiblings
	).

%%Return the effiency (%) for the current light
%%INPUTS: Dir: avenue, street or anyother that represents the current Direction to evaluate
%%		  SensorInputs: Data of the sensor for the current dir
%%		  NetValues: Output of the network for the current dir
%%OUTPUTS: effiency % for the current light on the indicated dir
%%DESC: Evaluate the realcount and the estimated_count for the current lane and get the effiency %
evaluate_effiency(Dir, SensorInputs, NetValues) ->
	io:format("[MODULER] EVALUATING EFFIENCY DIR: ~w SensorInputs: ~w, NetValues: ~w~n~n",[Dir, SensorInputs, NetValues]),
	SensorDirData = find_element(Dir, SensorInputs),
	NetDirValues = find_element(Dir, NetValues),
	
	%%Get the target elements real_count (sensor) and estimate_count (net)
	RealCount = find_element(real_count, SensorDirData),
	io:format("[MODULER] Getting estimatedcount from NetDirValues: ~w~n~n",[NetDirValues]),
	EstimatedCount = case NetDirValues of  [] -> 1; 
						  _Other -> Val = find_element(estimate_count, NetDirValues), 
									if Val == 0 -> 1; true -> Val end  end,
	
	io:format("[MODULER] Estimatedcount from NetDirValues: ~w~n~n",[EstimatedCount]),
	
	(RealCount / EstimatedCount) * 100.
	
%%%%%%%HACE ARREGLO EN MODULER AL FINAL DEL PROC PARA QUE AGARRE BIEN LOS DATOS Y LOS MAPEE POR DIRECCIONES
%%% ASI SE EVITA TENER QUE HACER GRANDES CAMBIOS POR TODO LADO
	
	
%evaluate_anomally_by_location(FilteredItems) ->
%	lists:map(fun({Location, Items}) ->
%			{Location, evaluate_anomally_by_location(Location, Items)}
%			end,
%			FilteredItems
%	).
%evaluate_anomally_by_location(Location, FilteredItems) ->
%	LocationItems = find_element(Location, FilteredItems),
%	CarCount = find_element(real_count, LocationItems),
%	Estimate = find_element(estimate_count, LocationItems),
%	
%	if CarCount =/= [], Estimate =/= [] -> CarCount - Estimate;
%			true  -> 0
%	end.


%format_result_for_interface(DirList, FinalDesition, Mapping) ->
%	Res = lists:map(
%		fun(Dir) ->
%			DirMappingValues = get_dir_dm_vals(Dir, Mapping),
%			DesitionsForDir = get_dir_desitions(DirMappingValues, FinalDesition),
%			{Dir, DesitionsForDir}
%		end,
%		DirList),
		
%	io:format("[MODULER] Formated Result ~w~n~n",[Res]),
%	FinalDesition.
	

%get_dir_dm_vals(Dir, MappList) ->
%	io:format("[MODULER] Dir ~w, MappList ~w~n~n", [Dir, MappList]),
%	{Positions, _LastPos} = lists:mapfoldl(fun(Item, AccPos) ->
%			{Key, _Value} = Item,
%			{R, Attribute} = break_key(Key),
%			if	R =:= Dir ->
%					{{Attribute, AccPos}, AccPos + 1};
%				true ->
%					{null, AccPos + 1}
%			end
%			end,
%			1,
%			MappList),
%	Res = [X || X <- Positions, X =/= null],
%	Res.


%%BREAKS down a key composed of "_" characters and gets the part before and after the _
%%NOTE: the key MUST follow the format dir_attribute e.g av_cycletime
%break_key(T) ->
%	L =  atom_to_list(T),
%	DirKey = list_to_atom(string:substr(L, 1, string:str(L, "_") - 1)),
%	Attribute = list_to_atom(string:substr(L, string:str(L, "_") + 1, length(L) )),
%	{DirKey, Attribute} .
%	
%get_list_item_by_index(Index, List) when Index < 1, Index > length(List) ->
%	null;
%get_list_item_by_index(Index, List) ->
%	lists:nth(Index, List).
%	
%get_dir_desitions(DirMappingValues, FinalDesition) ->
%	io:format("[MODULER] Getting desition values Index ~w FinalDesition ~w~n~n",[DirMappingValues, FinalDesition]),
%	lists:map(fun(Index) ->
%			Element = get_list_item_by_index(Index, FinalDesition),
%			Element
%			end,
%			DirMappingValues).
%	
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
			io:format("[MODULER] error"),
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

%%INPUT: ModulerPid related to the traffic light, 
%%		 Dir direction where to look for info
%%		 Stats, list of cars in the current lane
%%OUTPUT: returns the new data to use by the light (mostly times)
%%DESC: Called from Lights, in order to ask the DM (nn) what would be the next cycle
estimation_proc(ModulerPid, Dir, Stats) ->
	
	%ModulerPid ! {performance, self(), Dir, Stats},
	%receive
	%	{reply, ok} -> {reply, ok};
	%	_OtherPerformance ->	{error, moduler}		
	%end,
	
	ModulerPid ! {proc, self(), Dir, Stats},
	receive
		{reply, NewDesition} -> {reply, NewDesition};
		_OtherProc ->	{error, moduler}		
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
	{ModulerFile, _NNFile, _SensorLight, _NNLog} = CheckpointLog,
	%io:format("[MODULER] moduler file: ~p", [ModulerFile]),
	filemanager:write_raw(ModulerFile, io_lib:format("~w", [{LightId, FormatedData}])),
	write_checkpoint_nn(LightId, NN),
	write_checkpoint_sens(Sensor),
	timer:sleep(100).

write_checkpoint_nn(LightId, NN) ->
	NN ! {checkpoint, self()},
	io:format("[MODULER] Finished Saving NN for ~w ~n", [LightId]).

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
find_element(Id, Data) when is_list(Data) ->
	Element = lists:keyfind(Id, 1, Data),
	case Element of
		false ->	[];
		_Other ->	{Id, Value} = Element,
					Value
	end;
find_element(_Id, _Data) ->
	[].

find_child_element([], []) ->
	false;
find_child_element(_, []) ->
	false;
find_child_element([], _) ->
	false;
find_child_element([Parent | Hierachy], List) ->
	io:format("[MODULER] Looking for element ~w in ~w ~n",[Parent, List]),
	Element = find_element(Parent, List),
	io:format("[MODULER] Element found ~w rest of hierachy ~w~n",[Element, Hierachy]),
	case Element of
		[] -> false;
		_Other when Hierachy == [] -> Element;
		_Other -> find_child_element(Hierachy, Element)
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
