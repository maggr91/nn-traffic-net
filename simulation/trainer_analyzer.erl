-module(trainer_analyzer).
-export([start/0, evaluate/3]).

-export([init/0]).

start()->
	spawn(trainer, init, []).
	
init() ->
	%%start the trainer server (Used by all NN) in the network
	%%load the training sets
	TrainerReg = formated_log("/logs/training/training_rec_"),
	TrainingData = load_training_set(),
	io:format("Training server ONLINE~n",[]),
	train(TrainingData, TrainerReg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train(TrainingData, TrainerReg) ->
	receive
		{evaluate, CallerPid, TargetLane, CarsState} ->
			Res = evaluate_target(TargetLane, TrainerReg, CarsState, TrainingData),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg);
		killyou ->
		    {normal, ok}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     END  MAIN          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluate(TrainerPid, TargetLane, CarsState) ->
	TrainerPid ! {evaluate, self(), TargetLane, CarsState},
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


%%INPUT: CarList CurrentCars on lane,
%		 OldRecords cars records on textfile
%%OUTPUT: Average wait and delay time until now
%%DESC: use records to determine average times so far so the trainer can tell if there
%%		is a need for a change on the network
calculate_car_stats(CarList, OldRecords) ->
	MergedCars = lists:append(CarList, OldRecords),
	io:format("merged carlist ~w ~n~n",[MergedCars]),
	{SumWait, SumDelay} = lists:foldl(fun({_CarType,{Wait,Delay, _Position, _Route, _PrefLanes, _NextMove, _TopMove}}, {Res1, Res2}) -> 
		{Res1 + Wait, Res2 + Delay} end, {0,0}, MergedCars),
	Count = length(MergedCars),
	if Count > 0 ->
			{erlang:round(SumWait/Count) , erlang:round(SumDelay / Count)};
		true -> {SumWait, SumDelay}
	end.


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
