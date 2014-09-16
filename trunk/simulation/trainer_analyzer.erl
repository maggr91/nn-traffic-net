-module(trainer_analyzer).
-export([start/0, evaluate/3, evaluate/6]).

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
		{evaluate, CallerPid, TargetLane, CarsState, DMData, AnomalliesData, OutputMapping} ->
			io:format("Evaluate Performance~n"),
			DMCriteria = get_safe_element(criteria, Config),
			{Mapping, DecisionLogFile} = DMData,
			NewDecisionReg = update_decision_reg(DecisionLogFile, DecisionReg),
			%io:format("NewDecisionReg ~w~n",[NewDecisionReg]),
			Res = evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData, NewDecisionReg, 
									DMCriteria, Mapping, AnomalliesData, OutputMapping),
			io:format("Evaluate Target RES: ~w~n", [Res]),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg, DecisionReg, Config);
		{evaluate_intersection, CallerPid, TargetLanes, CarsStates, DMData, AnomalliesData, OutputMapping} ->
			io:format("Evaluate Performance for the intersection~n"),
			DMCriteria = get_safe_element(criteria, Config),
			FullCycle = get_safe_element(max_full_cycle, Config),
			{Mapping, DecisionLogFile} = DMData,
			NewDecisionReg = update_decision_reg(DecisionLogFile, DecisionReg),
			%io:format("NewDecisionReg ~w~n",[NewDecisionReg]),
			Res = evaluate_target_intersection(TargetLanes, TrainerReg, CarsStates, TrainingData, NewDecisionReg, DMCriteria, 
				Mapping, AnomalliesData, FullCycle, OutputMapping),
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
evaluate(TrainerPid, TargetLane, CarsState, DMData, AnomalliesData, OutputMapping) ->
	io:format("EVALUATING PERFORMANCE ~w~n~n",[{TrainerPid, TargetLane, CarsState, DMData, OutputMapping}]),	
	%TrainerPid ! {evaluate, self(), TargetLane, CarsState, DMData, AnomalliesData},
	TrainerPid ! {evaluate_intersection, self(), TargetLane, CarsState, DMData, AnomalliesData, OutputMapping},
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


evaluate_target(TargetLanes, TrainerReg, CarsStateList, TrainingData, DecisionReg, DMCriteria, DMMapping, AnomalliesData, _OutputMapping) ->
	CarStatsByLanes = lists:map(fun({_Dir, TargetLane}) ->
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
	FinalAdjustment = get_final_adjustment(TargetLanes, CarStatsByLanes, DecisionReg, DMCriteria, DMMapping, AnomalliesData),
	FinalAdjustment.


%%INPUTS: DMCriteria list of important aspects to evaluate in priority order (first most important)
%%		  DMMapping: maplist of DM outputs and what does each one represents
%%		  DecisionReg: list of all decisions taken so far by the DM
%%		  CarStatsByLanes: average estimation of times in cars by each lane (ac, av, etc)
%%OUTPUTS: Adjustment for the last decision taken by the DM
%%DESC: specific method to get the corresponding value
get_final_adjustment(TargetLanes, CarStatsByLanes, DecisionReg, DMCriteria, DMMapping, AnomalliesData) ->
	%io:format("~n~nGETTING Needed adjusment for light ~w ~n", [{TargetLanes, CarStatsByLanes, DecisionReg, DMCriteria, DMMapping}]),
	LastDecision = get_safe_last_reg(DecisionReg),
	io:format("~n~nLAST DECISION ~w ~n", [LastDecision]),
	case LastDecision of
		[] 		->	null;
		Other	-> 
					LastDecisionOutputs = get_safe_element(outputs, Other),
				  	OutputAdj = 
				  		lists:foldl(fun({Dir, TargetLane}, Res) ->
							DMValuesPos = get_dir_dm_vals(Dir, DMMapping),
							CarStatsforLane = get_safe_element(TargetLane, CarStatsByLanes),
							AnomalliesforLane = get_safe_element(Dir, AnomalliesData),
							io:format("DMValuesPos for ~w ~w~n~n",[{Dir, DMMapping}, DMValuesPos]),
							{FixedDecision, _EvalCriteria} = 
								check_criteria_single_dir(DMCriteria, DMValuesPos, Res, CarStatsforLane, AnomalliesforLane, []),
							
							FixedDecision
							%{Dir, TargetLane, {FixedDecision, EvalCriteria}}
							end,
							LastDecisionOutputs,
							TargetLanes),
					io:format("OutputAdj ~w~n",[OutputAdj]),
					Inputs = get_safe_list_element(inputs, Other),
					Outputs = decimals_to_binary(OutputAdj),
					io:format("TRAINING FIXES Inputs ~w OutputAdj ~w~n~n",[Inputs, Outputs]),
					%[{inputs, [1,0,1,0,1,1,0]} , {output, [1,1,1,0,1,1]}]
					[{inputs, Inputs} , {output, Outputs}]
	end.
	
	
%%INPUT: DMCriteria: this can be any criteria to evaluate in order to determine if the traffic is ok or not
%%		 some examples for each Dir {max_delay, 15}, {jam_in_back, true}
%%		 DMValuesPos: index inside the list of the elemets to be evaluated (wait, delay, etc).
%%		 LastDe
%%DESC: follow Criteria  list and determine the corresponding fix por
check_criteria_single_dir([], _DMValuesPos, _LastDecision,_CarStatsForLane, _AnomalliesData, _EvalCriteria) ->
	null;
check_criteria_single_dir([{avg_delay, Value} | Tail], DMValuesPos, LastDecision,CarStatsForLane, AnomalliesData, EvalCriteria) ->
	io:format("CarStatsForLane: ~w~n", [CarStatsForLane]),
	AvgDelay = get_safe_element(delay, CarStatsForLane),
	io:format("~nAvgDelay ~w Criteria Val: ~w~n",[AvgDelay, Value]),
	if	AvgDelay >= Value ->
			%%get the index
			CycleTimeIndex = get_safe_element(cycletime, DMValuesPos),
			%%get the value using the index
			CycleTime = lists:nth(CycleTimeIndex, LastDecision),
			FixedDecision = lists_replace(LastDecision, CycleTimeIndex, ceiling(CycleTime + (CycleTime * 0.4))),
			%FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + 1),
			{FixedDecision, EvalCriteria};
		true -> %%If it is low let it evaluate other criteria,
			check_criteria_single_dir(Tail, DMValuesPos, LastDecision,CarStatsForLane, AnomalliesData, [{avg_delay, false} | EvalCriteria])
	end;
				 
check_criteria_single_dir([{back_trouble_bit, true} | _Tail], DMValuesPos, LastDecision, _CarStatsForLane, AnomalliesData, EvalCriteria) ->
	%IsDelayOk = lists:any(fun({Key, _Value}) -> Key =:= avg_delay end, EvalCriteria),
	IsDelayPresent = lists:keyfind(avg_delay, 1, EvalCriteria),
	%%get the index
	CycleTimeIndex = get_safe_element(cycletime, DMValuesPos),
	%%get the value using the index 
	CycleTime = lists:nth(CycleTimeIndex, LastDecision),
	io:format("~nIsDelayOk ~w ... Anomallies: ~w~n",[IsDelayPresent, AnomalliesData]),
	{AnomalliesBefore, AnomalliesBeforeCount} = check_anomallies_by_location(antes, AnomalliesData),
	{AnomalliesAfter, AnomalliesAfterCount} = check_anomallies_by_location(despues, AnomalliesData),
	
	io:format("Anomallies before result ~w Anomallis afeter result~w~n~n",[{AnomalliesBefore, AnomalliesBeforeCount}, {AnomalliesAfter, AnomalliesAfterCount}]),
	
	{avg_delay, IsDelayOk} = IsDelayPresent,
	
	TimeFix = check_delay_with_anomallies(IsDelayOk, AnomalliesBefore, AnomalliesAfter),
	if CycleTime + TimeFix =< 0 ->
		NewTimeFix = TimeFix / 2,
		FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + NewTimeFix),
		{FixedDecision, [{back_trouble_bit, true} | EvalCriteria]};
	   true -> 
	    FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + TimeFix),
	   	{FixedDecision, [{back_trouble_bit, true} | EvalCriteria]}
		%false ->
		%	FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + 2),
		%	{FixedDecision, [{back_trouble_bit, true} | EvalCriteria]};			
		%{avg_delay, true} ->
		%	FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + 3),
		%	{FixedDecision, [{back_trouble_bit, true} | EvalCriteria]};
			
		%{avg_delay, false} ->
		%	FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + 1),
		%	{FixedDecision, [{back_trouble_bit, true} | EvalCriteria]};
		%_Other ->
		%	check_criteria_single_dir(Tail, DMValuesPos, LastDecision,CarStatsForLane, AnomalliesData, [{back_trouble_bit, true} | EvalCriteria])
	end;
		
check_criteria_single_dir([{back_trouble_bit, false} | Tail], DMValuesPos, LastDecision, CarStatsForLane, AnomalliesData, EvalCriteria) ->
	check_criteria_single_dir(Tail, DMValuesPos, LastDecision,CarStatsForLane, AnomalliesData, EvalCriteria);
check_criteria_single_dir([{effectiveness, Value} | Tail], DMValuesPos, LastDecision, CarStatsForLane, AnomalliesData, EvalCriteria) ->
	AnyOtherCriteria = length(EvalCriteria),
	IsDelayPresent = lists:keyfind(avg_delay, 1, EvalCriteria),
	case (AnyOtherCriteria == 0) of
		 true when IsDelayPresent == {avg_delay, false}; IsDelayPresent == {} ->
			CurrentEffictiveness = get_safe_element(effiency, AnomalliesData),
			Diff = Value - CurrentEffictiveness,
			CycleTimeIndex = get_safe_element(cycletime, DMValuesPos),
			CycleTime = lists:nth(CycleTimeIndex, LastDecision),
			TimeFix = if Diff > 5, Diff < 10 -> %%add 5 more seconds
					5;
			   Diff > 20 -> 10;
			   true -> 0
			end,
			FixedDecision = lists_replace(LastDecision, CycleTimeIndex, CycleTime + TimeFix),
	   		{FixedDecision, [{back_trouble_bit, true} | EvalCriteria]};
	  	 false ->
	   		check_criteria_single_dir(Tail, DMValuesPos, LastDecision,CarStatsForLane, AnomalliesData, [{effectiveness, false} | EvalCriteria])
	end;
check_criteria_single_dir(_DMCriteria, _DMValuesPos, _LastDecision, _CarStatsForLane, _AnomalliesData, _EvalCriteria) ->
%	recorrer la lista de criterios y hacer un case para tratar de evaluar cada uno de forma dif... si se cumple el primero
%	se retorna, si no se continua
	true.
	


%%Eval delay with anomallies to see if more or less time is required to reduce the avg delay
check_delay_with_anomallies(true, true, true) ->
	-1;
check_delay_with_anomallies(true, false, true) ->
	1;
check_delay_with_anomallies(true, true, false) ->
	5;
check_delay_with_anomallies(false, true, true) ->
	1;
check_delay_with_anomallies(false, false, true) ->
	2;
check_delay_with_anomallies(false, true, false) ->
	10;
check_delay_with_anomallies(false, false, false) ->
	0;
check_delay_with_anomallies(_IsAvgDelayPresent, _AnomallyBefore, _AnomallyAfter) ->
	20.


%%=========================================================================================================
%%============================EVALUATE FOR INTERSECTION====================================================	

evaluate_target_intersection(TargetLanes, TrainerReg, CarsStateList, _TrainingData, DecisionReg, DMCriteria, 
  DMMapping, AnomalliesData, FullCycle, OutputMapping) ->
	CarStatsByLanes = lists:map(fun({_Dir, TargetLane}) ->
			FinalReg = TrainerReg ++ string:concat(atom_to_list(TargetLane), ".txt"),
			%io:format("~n~nTrainer lookup file: ~p~n~n",[FinalReg]),
	
			%Target = get_safe_element(TargetLane, TrainingData),
			%io:format("Target lookup: ~w~n~n",[Target]),
	
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
	FinalAdjustment = get_final_adjustment_intersection(TargetLanes, CarStatsByLanes, DecisionReg, DMCriteria, 
		DMMapping, AnomalliesData, trunc(FullCycle / length(CarStatsByLanes)), OutputMapping),
	FinalAdjustment.


%%INPUTS: DMCriteria list of important aspects to evaluate in priority order (first most important)
%%		  DMMapping: maplist of DM outputs and what does each one represents
%%		  DecisionReg: list of all decisions taken so far by the DM
%%		  CarStatsByLanes: average estimation of times in cars by each lane (ac, av, etc)
%%OUTPUTS: Adjustment for the last decision taken by the DM
%%DESC: specific method to get the corresponding value
get_final_adjustment_intersection(TargetLanes, CarStatsByLanes, DecisionReg, DMCriteria, DMMapping, 
  AnomalliesData, RefNormalTimePerLane, OutputMapping) ->
	%io:format("~n~nGETTING Needed adjusment for light ~w ~n", [{TargetLanes, CarStatsByLanes, DecisionReg, DMCriteria, DMMapping}]),
	LastDecision = get_safe_last_reg(DecisionReg),
	io:format("~n~nLAST DECISION ~w ~n", [LastDecision]),
	case LastDecision of
		[] 		->	null;
		Other	-> 
					LastDecisionOutputs = get_safe_element(outputs, Other),
					SortedCarStatsDesc = sort(CarStatsByLanes, desc),
					
					io:format("~n~nSorted Car stats ~w ~n", [SortedCarStatsDesc]),
					DirDmVals = lists:map(fun ({Dir, TargetLane}) -> {TargetLane, get_dir_dm_vals(Dir, DMMapping)} end, TargetLanes),
					
					io:format("Direction values ~w ~n", [DirDmVals]),
					SortedCarStatsDescWithAdj = get_adjustment_combined_values(SortedCarStatsDesc, DirDmVals, DMCriteria, 
													AnomalliesData, LastDecisionOutputs, RefNormalTimePerLane),
					%%update last decision with the corresponding adjustments
					
					io:format("SortedCarStatsDescWithAdj ~w ~n", [SortedCarStatsDescWithAdj]),
					
					OutputAdj  = 
						lists:foldl(fun({_Dir, Data}, Res) ->
							DMValuesPos = get_safe_element(val_pos, Data),
							CycleTimeIndex = get_safe_element(cycletime, DMValuesPos),
							%CycleTime = lists:nth(CycleTimeIndex, Res),
							CycleTime = get_safe_element(cycletime, Data),
							Adjustment = get_safe_element(adjust, Data),
							NewCycleTime = ceiling(CycleTime + (CycleTime * Adjustment)), %truncate_time(RefNormalTimePerLane, CycleTime),
							io:format("CycleTime ~w Adjustment ~w~n",[CycleTime, Adjustment]),
							io:format("NewCycleTime ~w OldCycleTime ~w~n",[NewCycleTime, CycleTime]),
							%%Change the old value for the current direction inside the lastDecision list
							lists_replace(Res, CycleTimeIndex, NewCycleTime) 
							end,
							LastDecisionOutputs,
							SortedCarStatsDescWithAdj
						),
						
					%OutputAdj  = 
					%	lists:foldl(fun({_Dir, Data}, Res) ->
					%		DMValuesPos = get_safe_element(val_pos, Data),
					%		CycleTimeIndex = get_safe_element(cycletime, DMValuesPos),
					%		%CycleTime = lists:nth(CycleTimeIndex, Res),
					%		CycleTime = get_safe_element(cycletime, Data),
					%		Adjustment = get_safe_element(adjust, Data),
					%		NewCycleTime = ceiling(CycleTime + (CycleTime * Adjustment)),
					%		io:format("CycleTime ~w Adjustment ~w~n",[CycleTime, Adjustment]),
					%		io:format("NewCycleTime ~w OldCycleTime ~w~n",[NewCycleTime, CycleTime]),
					%		%%Change the old value for the current direction inside the lastDecision list
					%		lists_replace(Res, CycleTimeIndex, NewCycleTime) 
					%		end,
					%		LastDecisionOutputs,
					%		SortedCarStatsDescWithAdj
					%	),				  	
					io:format("OutputAdj ~w~n",[OutputAdj]),
					Inputs = get_safe_list_element(inputs, Other),
					Outputs = decimals_to_binary(OutputAdj, OutputMapping),
					io:format("TRAINING FIXES Inputs ~w OutputAdj ~w~n~n",[Inputs, Outputs]),
					%[{inputs, [1,0,1,0,1,1,0]} , {output, [1,1,1,0,1,1]}]
					[{inputs, Inputs} , {output, Outputs}]
	end.


%%%Get time fix for all using fixed adjustments
get_adjustment_combined_values([], _DirDmValsPos, _DMCriteria, _AnomalliesData, _LastDecisionOutputs, _RefNormalTimePerLane) ->
	[];
get_adjustment_combined_values(CarStatsByLanes, DirDmValsPos, DMCriteria, AnomalliesData, LastDecisionOutputs, RefNormalTimePerLane) ->
	AdjustmentFirst = 0.00,
	[MostAffected | _LessAffected] = CarStatsByLanes, 
	CarStatsWithValuePos = lists:map(
		fun({Dir, Data}) -> 
			ValsPos = get_safe_list_element(Dir, DirDmValsPos),
			CycleTimeIndex = get_safe_element(cycletime, ValsPos),
			CycleTime = lists:nth(CycleTimeIndex, LastDecisionOutputs),
			if 	MostAffected == {Dir, Data} ->
					{Dir, [{val_pos, ValsPos}, {adjust, AdjustmentFirst}, {cycletime, CycleTime} | Data]};
				true ->
					{Dir, [{val_pos, ValsPos}, {adjust, -AdjustmentFirst}, {cycletime, CycleTime} | Data]}
			end
		end,
		CarStatsByLanes),
		
	IsBoostAllowed = is_boost_allowed(CarStatsWithValuePos, RefNormalTimePerLane),
	%ListWithOutFirst = lists:sublist(CarStatsWithValuePos, 2, length(CarStatsWithValuePos) - 1),
	
	%OLD WAY 11/5/2014 UNCOMMENT if necessary
	TempListWithCriteriaCheck = 
		if IsBoostAllowed == false ->
				io:format("NO BOOST just check truncate TImes ~w ~n",[CarStatsWithValuePos]),
				fix_intersection_times(CarStatsWithValuePos, RefNormalTimePerLane, truncate);	
			true ->
				%% if the lights has not yet reached the max ref value for time boost all of them
				io:format("Boosting TImes ~w ~n",[CarStatsWithValuePos]),
				Res = fix_intersection_times(CarStatsWithValuePos, RefNormalTimePerLane, boost),
				io:format("Boosted TImes ~w ~n~n",[Res]),
				Res
		end,
		
	io:format("~nCarStatsWithValuePos ~w ~n", [TempListWithCriteriaCheck]),
	[UPDMostAffected | ListWithOutFirst] = TempListWithCriteriaCheck,
		
	io:format("~ncheck_criteria_intersection ~w ~n", [{DMCriteria, UPDMostAffected, 
		ListWithOutFirst, AnomalliesData}]),
		
	io:format("Checking criteria Times SECOND boosting ~n~n",[]),
	ListWithCriteriaCheck = check_criteria_intersection(DMCriteria, UPDMostAffected, 
								ListWithOutFirst, AnomalliesData, []),
								
	io:format("~nListWithCriteriaCheck ~w ~n", [ListWithCriteriaCheck]),
	ListWithCriteriaCheck.

%%INPUT: DMCriteria: this can be any criteria to evaluate in order to determine if the traffic is ok or not
%%		 some examples for each Dir {max_delay, 15}, {jam_in_back, true}
%%		 DMValuesPos: index inside the list of the elemets to be evaluated (wait, delay, etc).
%%		 LastDe
%%DESC: follow Criteria  list and determine the corresponding fix por
check_criteria_intersection([], MainAffected, OtherAffected, _AnomalliesData, _EvalCriteria) ->
	[MainAffected | OtherAffected];
check_criteria_intersection([{avg_delay, Value} | Tail], MainAffected, OtherAffected, AnomalliesData, EvalCriteria) ->
	{Dir, Data} = MainAffected,
	Adjustment = get_safe_element(adjust, Data),
	AvgDelay = get_safe_element(delay, Data),
	Length = length(OtherAffected),
	MaxAdjust = Length * 0.5,
	io:format("~n~n AvgDelay ~w  Value ~w ~n", [AvgDelay, Value]),
	
	%%%PREGUNTAR SI VALUE ES MAYOR ENTONCES RESTAR EL 1 SI NO NO
	%Difference = 
	%	if AvgDelay > Value ->	abs(AvgDelay / Value - 1);
	%		true ->				abs(AvgDelay / Value)
	%	end,
	%NewAdjust  = 
	%	if 	Difference >= MaxAdjust -> MaxAdjust;
	%		true ->
	%			Difference
	%	end,
	NewAdjust = get_new_adjustment(AvgDelay / Value, MaxAdjust),
	io:format("~n~n MaxAdjust ~w  NewAdjust~w ~n", [MaxAdjust, NewAdjust]),
	NewData = lists:keyreplace(adjust,1, Data, {adjust, Adjustment + NewAdjust}),
	NewOtherAffected = update_adjustment_for_others(OtherAffected, NewAdjust / Length),
	check_criteria_intersection(Tail, {Dir, NewData}, NewOtherAffected, AnomalliesData, EvalCriteria);

check_criteria_intersection([{back_trouble_bit, true} | Tail], MainAffected, OtherAffected, AnomalliesData, EvalCriteria) ->
	check_criteria_intersection(Tail, MainAffected, OtherAffected, AnomalliesData, EvalCriteria);
check_criteria_intersection([{back_trouble_bit, false} | Tail], MainAffected, OtherAffected, AnomalliesData, EvalCriteria) ->
	check_criteria_intersection(Tail, MainAffected, OtherAffected, AnomalliesData, EvalCriteria);
check_criteria_intersection([{effectiveness, _Value} | Tail], MainAffected, OtherAffected, AnomalliesData, EvalCriteria) ->
	check_criteria_intersection(Tail, MainAffected, OtherAffected, AnomalliesData, EvalCriteria);
check_criteria_intersection(_DMCriteria, _MainAffected, _OtherAffected, _AnomalliesData, _EvalCriteria) ->
%	recorrer la lista de criterios y hacer un case para tratar de evaluar cada uno de forma dif... si se cumple el primero
%	se retorna, si no se continua
	true.

update_adjustment_for_others(List, Adjustment) ->
	lists:map(
		fun({Dir, Data}) ->
			Res = update_elements([{value, adjust, Adjustment * -1}], Data, null),
			{Dir, Res}
		end,
		List
	).
			
get_new_adjustment(TimePercentaje, MaxAdjust) when TimePercentaje > 1 ->
	Difference = TimePercentaje - 1,
	%%get fixed adjustment for over 
	FixEvery40 = (Difference * 100) / 40,
	if	FixEvery40 < 1 ->
			0.2;
		true ->
			get_new_adjustment_aux(trunc(round(FixEvery40)) * 0.2, MaxAdjust)
	end;
	
get_new_adjustment(_TimePercentaje, _MaxAdjust) ->
	0.

get_new_adjustment_aux(TempFix, MaxAdjust) when TempFix > MaxAdjust ->
	MaxAdjust;
get_new_adjustment_aux(TempFix, _MaxAdjust) ->
	TempFix.


%%return true if it is possible to boost all lanes because they can go up more
is_boost_allowed(List, RefTime) ->
	lists:all(fun({_Dir, Data}) -> CycleTime = get_safe_element(cycletime, Data),
			CycleTime < RefTime end, List).

%%When there is a window to boost time (until a limit i.e 30 seconds), boost a 20% of current time
%%Mode = boost (add time) or truncate (reduce to limit)
fix_intersection_times(List, TimeLimit, Mode) ->
	fix_intersection_times(List, TimeLimit, Mode, []).
fix_intersection_times([], _TimeLimit, _Mode, UpdatedList) ->
	lists:reverse(UpdatedList);
fix_intersection_times([{Dir, Data} | Tail], TimeLimit, Mode, UpdatedList) ->
	CycleTime = get_safe_element(cycletime, Data),
	io:format("Fix mode ~w, CycleTime ~w , TimeLimit ~w ~n~n",[Mode, CycleTime,TimeLimit]),
	NewCycleTime = fix_time_mode(Mode, CycleTime,TimeLimit),
	NewData = lists:keyreplace(cycletime, 1, Data, {cycletime, NewCycleTime}),
	fix_intersection_times(Tail, TimeLimit, Mode, [{Dir, NewData} |  UpdatedList]).


fix_time_mode(truncate, Time, Limit) ->
	truncate_time(Limit, Time);
fix_time_mode(boost, Time, Limit) ->
	truncate_time(Limit, boost_time_aux(Time)).

boost_time_aux(0) ->
	30;
boost_time_aux(Time) ->
	ceiling(Time + (Time * 0.9)).
	

%%=======================END OF EVALUATE FOR INTERSECTION=================================================
%%========================================================================================================	

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

reply(ReplyKey, Pid, Reply) ->
    Pid ! {ReplyKey, Reply}.
    
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
%%		 OutputMapping -> list of keys/bits represented by each decimal value
%%OUTPUT: List of numbers converted to binary (1 o 0)
%%DESC: Gets a sequence of numbers in a list, convert each one to binary and return a list in order 
%%		where a group of 1 and 0 represents the first number, the next group the second number and so on
decimals_to_binary(List) ->
	T = lists:reverse(lists:foldl(fun(E, Res) -> Bin = convert_to_binary(E), [Bin | Res] end, [], List)),
	lists:append(T).
	
decimals_to_binary(List, OrigOutputMapping) ->
	[{repeat, N} | OutputMapp] = OrigOutputMapping,
	OutputMapping = lists:append(lists:duplicate(N, OutputMapp)),
	io:format("DEBUG: decimals_to_binary for ~w OutputMapping ~w ~n", [List, OutputMapping]),
	lists:append(decimals_to_binary_helper(OutputMapping, List, [])).

decimals_to_binary_helper([], _List, AccList) ->
	lists:reverse(AccList);
decimals_to_binary_helper([KeyMap | TailMap], [DecimalValue | TailList], AccList) ->
	{_Key, Bits} = KeyMap,
	BitsValue = convert_to_fixed_binary(DecimalValue, Bits),
	decimals_to_binary_helper(TailMap, TailList, [BitsValue | AccList]).
	
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
	
%%Gets all keys inside a list of tuples {key, value}
%convert_to_keys(List) ->
%	lists:map(fun({Key, _Value}) -> Key end, List).
	
get_config() ->
	filemanager:get_data("/config/config_analyzer.txt").
	
get_dir_dm_vals(Dir, MappList) -> 
	{Positions, _LastPos} = lists:mapfoldl(fun(Item, AccPos) ->
			{Key, _Value} = Item,
			{R, Attribute} = break_key(Key),
			if	R =:= Dir ->
					{{Attribute, AccPos}, AccPos + 1};
				true ->
					{null, AccPos + 1}
			end
			end,
			1,
			MappList),
	Res = [X || X <- Positions, X =/= null],
	Res.


%%BREAKS down a key composed of "_" characters and gets the part before and after the _
%%NOTE: the key MUST follow the format dir_attribute e.g av_cycletime
break_key(T) ->
	L =  atom_to_list(T),
	DirKey = list_to_atom(string:substr(L, 1, string:str(L, "_") - 1)),
	Attribute = list_to_atom(string:substr(L, string:str(L, "_") + 1, length(L) )),
	{DirKey, Attribute} .

get_safe_last_reg([]) ->
	[];
get_safe_last_reg(List) ->
	lists:last(List).
	
%%REPLACE value on index
%lists_replace(List, Indx, Value) ->
%	Length = length(List),
%	if Indx =< Length, Indx >= 1 ->
%		lists_replace_aux(List, Indx, Value, 1, []);
%	   true ->
%	   	List
%	end.
	
%lists_replace_aux([_Item | List], Indx, Value, CurrIndx, AccList) when Indx == CurrIndx ->
%	ListPart = lists:reverse([Value | AccList]),
%	lists:append(ListPart, List);
	
%lists_replace_aux([Item | List], Indx, Value, CurrIndx, AccList) when Indx > CurrIndx -> 
%	lists_replace_aux(List, Indx, Value, CurrIndx + 1, [Item | AccList]).
	

%%REPLACE value on index
%% lists_replace(List, Index, NewElement) -> List.
lists_replace([_|Rest], 1, New) -> [New|Rest];
lists_replace([E|Rest], I, New) -> [E|lists_replace(Rest, I-1, New)].


%check_anomallies_by_location(Location, AnomalliesData, Key) ->
%	LocationAnomallies = get_safe_element(Location, AnomalliesData),
%	Anomallies = get_safe_element(Key, AnomalliesData),
%	case Anomallies of
%		[] -> false;
%		_Other -> lists:any(fun() -> Value == 1 end,Anomallies)	
%	end.

check_anomallies_by_location(Location, AnomalliesData) ->
	Anomallies = get_safe_list_element(Location, AnomalliesData),
	get_safe_anomally(Anomallies, false, 0).
				
get_safe_anomally([], ExistsAnomally, AnomalliesCount) ->
	{ExistsAnomally, AnomalliesCount};
get_safe_anomally([{_LightId, Value} | Tail], _ExistsAnomally, AnomalliesCount) when Value == 1; Value == true ->
	get_safe_anomally(Tail, true, AnomalliesCount + 1);
get_safe_anomally([_Item| Tail], ExistsAnomally, AnomalliesCount)->
	get_safe_anomally(Tail, ExistsAnomally, AnomalliesCount).
	

%%%ORDER ELEMENTS IN A LIST
sort(List) ->
	sort(List, asc).
sort(List, desc) ->
	lists:sort(fun({_KeyA, ValueA}, {_KeyB, ValueB}) -> ValueA > ValueB end, List);
sort(List, _Order) ->
	lists:sort(fun({_KeyA, ValueA}, {_KeyB, ValueB}) -> ValueA < ValueB end, List).


%%%%%UPDATE ELEMENTS
update_elements(List, OldRecordList, null) ->
	update_elements_aux(List, OldRecordList);
update_elements(List, OldRecordList, Parent) ->
	TargetList = get_safe_element(Parent, OldRecordList),
	R = update_elements_aux(List, TargetList),
	%io:format("Res = ~w~n~n", [R]),
	lists:keyreplace(Parent, 1, OldRecordList, {Parent, R}).

update_elements_aux([], UpdatedRecordList) ->
	UpdatedRecordList;
update_elements_aux([Element | Tail], OldRecordList) ->
	UpdatedRecordList = update_element(Element, OldRecordList),
	update_elements_aux(Tail, UpdatedRecordList).
	

update_element({lists, Key, []}, OldRecordList) ->
	lists:keyreplace(Key, 1, OldRecordList, {Key, []});
update_element({lists, Key, Value}, OldRecordList) ->
	OldValue = get_safe_element(Key, OldRecordList),
	NewValue = [Value | OldValue],
	lists:keyreplace(Key, 1, OldRecordList, {Key, NewValue});
	
update_element({lists_replace, Key, []}, OldRecordList) ->
	lists:keyreplace(Key, 1, OldRecordList, {Key, []});
update_element({lists_replace, Key, NewValue}, OldRecordList) ->
	lists:keyreplace(Key, 1, OldRecordList, {Key, NewValue});

update_element({value_replace, Key, NewValue}, OldRecordList) ->
	lists:keyreplace(Key, 1, OldRecordList, {Key, NewValue});
update_element({value, Key, Value}, OldRecordList) ->
	OldValue = get_safe_element(Key, OldRecordList),
	NewValue = Value + OldValue,
	lists:keyreplace(Key, 1, OldRecordList, {Key, NewValue}).
	
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
    
%%Return the value of limit or the corresponding value if not exceed
truncate_time(Limit, Value) when Value > Limit ->
	Limit;
truncate_time(_Limit, Value) ->
	Value.
	
