-module(trainer_analyzer).
-export([start/0, evaluate/3, evaluate/4]).

-export([init/0]).

start()->
	spawn(trainer_analyzer, init, []).
	
init() ->
	%%start the trainer server (Used by all NN) in the network
	%%load the training sets
	TrainerReg = formated_log("/logs/training/training_rec_"),
	TrainingData = load_training_set(),
	train(TrainingData, TrainerReg, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train(TrainingData, TrainerReg, DecisionReg) ->
	receive
		{evaluate, CallerPid, TargetLane, CarsState} ->
			Res = evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg, DecisionReg);
		{evaluate, CallerPid, TargetLane, CarsState, DMData} ->
			io:format("Evaluate Performance~n"),
			
			{_Mapping, DecisionLogFile} = DMData,
			NewDecisionReg = update_decision_reg(DecisionLogFile, DecisionReg),
			io:format("NewDecisionReg ~w~n",[NewDecisionReg]),
			Res = evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData, NewDecisionReg),
			io:format("Evaluate Target RES: ~w~n", [Res]),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg, DecisionReg);
		test ->
			io:format("CALL TEST~n", []);
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


evaluate_target(TargetLanes, TrainerReg, CarsState, TrainingData, DecisionReg) ->
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
			CarStats = calculate_car_stats(CarsState, OldRecords), %%SumWait, SumDelay, etc (a list)
	
			%%
			%io:format("~nAveWait ~w, AveDelay ~w ~n~n",[SumWait, SumDelay]),
			{TargetLane, CarStats}
			end,
			TargetLanes),
	io:format("~n~nCARSTATS BY LANES ON ANALYZER ~w~n~n",[CarStatsByLanes]),
	
	[{inputs, [20,0,1,0,1,1,0]} , {output, [1,1,1,0,1,1]}].


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
		
