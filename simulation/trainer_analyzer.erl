-module(trainer_analyzer).
-export([start/0, evaluate/3, evaluate/4]).

-export([init/0]).

start()->
	spawn(trainer_analyzer, init, []).
	
init() ->
	%%start the trainer server (Used by all NN) in the network
	%%load the training sets
	TrainerReg = formated_log("/logs/training/training_rec_"),
	
	%%load corresponding configuration (used by all analyzers)
	ConfigData = get_config(),
	Config = get_safe_element(config, ConfigData),
	TrainingData = load_training_set(),
	train(TrainingData, TrainerReg, [], Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train(TrainingData, TrainerReg, DecisionReg, Config) ->
	receive
		{evaluate, CallerPid, TargetLane, CarsState} ->
			Res = evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg, DecisionReg, Config);
		{evaluate, CallerPid, TargetLane, CarsState, DMData} ->
			io:format("Evaluate Performance~n"),
			DMCriteria = get_safe_element(criteria, Config),
			{Mapping, DecisionLogFile} = DMData,
			NewDecisionReg = update_decision_reg(DecisionLogFile, DecisionReg),
			io:format("NewDecisionReg ~w~n",[NewDecisionReg]),
			Res = evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData, NewDecisionReg, DMCriteria, Mapping),
			io:format("Evaluate Target RES: ~w~n", [Res]),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg, DecisionReg, Config);
		test ->
			io:format("CALL TEST ~w~n", [Config]),
			train(TrainingData, TrainerReg, DecisionReg, Config);
		killyou ->
		    {normal, ok}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     END  MAIN          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%INPUTS: TrainerPid: PID of targe trainer
%%		  TargetLane: list of the lanes to evaluate
%%		  CarsState: list of cars with statistics
%%OUTPUTS: List of ajust that the DM should apply [{inputs, [...]}, {output, [...]}]
%%DESC:   Get all data from current state an evaluate if the results are good if not, do a ajustment 
evaluate(TrainerPid, TargetLane, CarsState) ->
	TrainerPid ! {evaluate, self(), TargetLane, CarsState},
	receive
		{reply, Return} -> Return;
		_Other			-> {error, trainer}		
	end.

%%INPUTS: TrainerPid: PID of targe trainer
%%		  TargetLane: list of the lanes to evaluate
%%		  CarsState: list of cars with statistics
%%		  DMData: data related to DM used for analisys
%%OUTPUTS: List of ajust that the DM should apply [{inputs, [...]}, {output, [...]}]
%%DESC:   Get all data from current state an evaluate if the results are good if not, do a ajustment 
evaluate(TrainerPid, TargetLane, CarsState, DMData) ->
	io:format("EVALUATING PERFORMANCE ~w~n~n",[{TrainerPid, TargetLane, CarsState, DMData}]),
	TrainerPid ! {evaluate, self(), TargetLane, CarsState, DMData},
	%TrainerPid ! test,
	receive
		{reply, Return} -> Return;
		_Other			-> {error, trainer}		
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     GENERAL FUNCTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData) ->
	FinalReg = TrainerReg ++ string:concat(atom_to_list(TargetLane), ".txt"),
	io:format("~n~nTrainer lookup file: ~p~n~n",[FinalReg]),
	
	Target = get_safe_element(TargetLane, TrainingData),
    io:format("Target lookup: ~w~n~n",[Target]),
	
	OldRecords =
		try
			filemanager:get_data_by_fullpath(FinalReg)
			%io:format("~n~n~n~nTRAINER Data found: ~w ~n~n~n~n",[OldRecords])
		catch 
		%exit : { noproc, _ } -> closed
			Exception:Reason -> io:format("NO TRAINER Data found. Exception ~p , Reason ~p ~n",[Exception, Reason]),
								[]
    	end,
		
	%%Eval cars data 
	{SumWait, SumDelay} = calculate_car_stats(CarsState, OldRecords),
	
	%%
	io:format("~nAveWait ~w, AveDelay ~w ~n~n",[SumWait, SumDelay]),
	true.


evaluate_target(TargetLanes, TrainerReg, CarsStateList, TrainingData, DecisionReg, DMCriteria, DMMapping) ->
	CarStatsByLanes = lists:map(fun(TargetLane) ->
			FinalReg = TrainerReg ++ string:concat(atom_to_list(TargetLane), ".txt"),
			io:format("~n~nTrainer lookup file: ~p~n~n",[FinalReg]),
	
			Target = get_safe_element(TargetLane, TrainingData),
			io:format("Target lookup: ~w~n~n",[Target]),
	
			OldRecords =
				try
					filemanager:get_data_by_fullpath(FinalReg)
					%io:format("~n~n~n~nTRAINER Data found: ~w ~n~n~n~n",[OldRecords])
				catch 
				%exit : { noproc, _ } -> closed
					Exception:Reason -> io:format("NO TRAINER Data found. Exception ~p , Reason ~p ~n",[Exception, Reason]),
										[]
				end,
		
			%%Eval cars data
			CarsState = get_safe_list_element(TargetLane, CarsStateList),
			CarStats = calculate_car_stats(CarsState, OldRecords), %%SumWait, SumDelay, etc (a list)
	
			%%
			%io:format("~nAveWait ~w, AveDelay ~w ~n~n",[SumWait, SumDelay]),
			{TargetLane, CarStats}
			end,
			TargetLanes),
	io:format("~n~nCARSTATS BY LANES ON ANALYZER ~w~n~n",[CarStatsByLanes]),
	FinalAdjustment = get_final_adjustment(CarStatsByLanes, DecisionReg, DMCriteria, DMMapping),
	FinalAdjustment.


%%INPUTS: DMCriteria list of important aspects to evaluate in priority order (first most important)
%%		  DMMapping: maplist of DM outputs and what does each one represents
%%		  DecisionReg: list of all decisions taken so far by the DM
%%		  CarStatsByLanes: average estimation of times in cars by each lane (ac, av, etc)
%%OUTPUTS: Adjustment for the last decision taken by the DM
%%DESC: specific method to get the corresponding value
get_final_adjustment(CarStatsByLanes, DecisionReg, DMCriteria, DMMapping) ->
	io:format("~n~nGETTING Needed adjusment for light ~w ~n", [{CarStatsByLanes, DecisionReg}]),
	LastDecision = lists:last(DecisionReg),
	
	[{inputs, [1,0,1,0,1,1,0]} , {output, [1,1,1,0,1,1]}].
	
	
%%INPUT: DMCriteria: this can be any criteria to evaluate in order to determine if the traffic is ok or not
%%		 some examples for each Dir {max_delay, 15}, {jam_in_back, true}

%%DESC: seguir cada
check_criteria(DMCriteria, DMMapping, LastDecision) ->
%	recorrer la lista de criterios y hacer un case para tratar de evaluar cada uno de forma dif... si se cumple el primero
%	se retorna, si no se continua
	true.

%%INPUT: CarList CurrentCars on lane,
%		 OldRecords cars records on textfile
%%OUTPUT: Average wait and delay time until now
%%DESC: use records to determine average times so far so the trainer can tell if there
%%		is a need for a change on the network
calculate_car_stats(CarList, OldRecords) ->
	MergedCars = lists:append(CarList, OldRecords),
	%io:format("merged carlist ~w ~n~n",[MergedCars]),
	Default = [{wait, 0},{delay, 0}],
	Keys = [wait, delay],
	%{SumWait, SumDelay} = lists:foldl(fun({_CarType,{Wait,Delay, _Position, _Route, _PrefLanes, _NextMove, _TopMove}}, {Res1, Res2}) -> 
	Res = lists:foldl(fun({_CarType,{_Position, _NextMove, _PrefLanes, Records}}, Acc) ->
		 Times = get_safe_element(times, Records),
		 NewAcc = acc_result(Times, Keys, Acc),
		 NewAcc end, Default, MergedCars),
		 %{Res1 + Wait, Res2 + Delay} end, {0,0}, MergedCars),
	Count = length(MergedCars),
	if Count > 0 ->
			post_acc_result(Res, Count);
		true -> Res
	end.

acc_result(Records, Keys, Acc) ->
	lists:map(fun(Key) ->
		KeyValue = get_safe_element(Key, Records),
		AccValue = get_safe_element(Key, Acc),
		
		NewValue = try
					KeyValue + AccValue
				catch
					Exception:Reason -> AccValue
				end,
		{Key, NewValue}
		end,
		Keys
	).

post_acc_result(Acc, Count) ->
	lists:map(fun(Item) ->
			{Key, SumItem} = Item,
			{Key, erlang:round(SumItem/Count)}
		end,
		Acc
	).
		


load_training_set() ->
	filemanager:get_data("/ann/training/training_set.txt").
	
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.
    
formated_log(File) ->
	{ok, Cwd} = file:get_cwd(),
	Cwd ++ File.
	
get_safe_element(Key, List) ->
	Element = lists:keyfind(Key, 1, List),
	case Element of
		false -> {};
		{Key, Value} -> Value	
	end.
	
get_safe_list_element(Key, List) ->
	Element = lists:keyfind(Key, 1, List),
	case Element of
		false -> [];
		{Key, Value} -> Value	
	end.
	
update_decision_reg(DecisionLogFile, DecisionReg) ->
	OldRecords = 
		try
			filemanager:get_data_by_fullpath(DecisionLogFile)
		catch 
		%exit : { noproc, _ } -> closed
			Exception:Reason -> io:format("NO decision log found or error when loading data found. Exception ~p , Reason ~p ~n",[Exception, Reason]),
								DecisionReg
    	end,
    OldRecords.


%%INPUT: List -> list of numbers to convert
%%OUTPUT: List of numbers converted to binary (1 o 0)
%%DESC: Gets a sequence of numbers in a list, convert each one to binary and return a list in order 
%%		where a group of 1 and 0 represents the first number, the next group the second number and so on
decimals_to_binary(List) ->
	T = lists:reverse(lists:foldl(fun(E, Res) -> Bin = convert_to_binary(E), [Bin | Res] end, [], List)),
	lists:append(T).
		
convert_to_binary(Value) ->
	L = integer_to_list(Value, 2),
	lists:map(fun(Item) -> list_to_integer([Item]) end, L).
	
%%Gets all keys inside a list of tuples {key, value}
convert_to_keys(List) ->
	lists:map(fun({Key, _Value}) -> Key end, List).
	
get_config() ->
	filemanager:get_data("/config/config_analyzer.txt").
