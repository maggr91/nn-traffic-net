-module(trainer).
-export([start/0, get_log/1, evaluate/2, update/3]).

-export([init/0]).

start()->
	spawn(trainer, init, []).
	
init() ->
	%%start the trainer server (Used by all NN) in the network
	%%load the training sets
	TrainerReg = formated_log("/logs/training_set.txt"),
	TrainingData = load_training_set(),
	%register(network, ann:start()),
	%timer:apply_after(200, trainer, train, [TrainingData, TrainerReg]).
	io:format("Training server ONLINE~n",[]),
	train(TrainingData, TrainerReg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train(TrainingData, TrainerReg) ->
	receive
		{get_log, CallerPid} ->
			reply(CallerPid, TrainerReg),
			train(TrainingData, TrainerReg);
		{evaluate, CallerPid, TargetLane} ->
			Res = evaluate_target(TargetLane, TrainerReg),
			reply(CallerPid, Res),
			train(TrainingData, TrainerReg);
		{rec, Data, _Location} ->
			io:format("Trainer Data ~w",[Data]),
			write(TrainerReg, Data),
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
get_log(TrainerPid) ->
	TrainerPid ! {get_log, self()},
	receive
		{reply, Return} -> Return;
		_Other			-> {error, trainer}		
	end.


evaluate(TrainerPid, TargetLane) ->
	TrainerPid ! {evaluate, self(), TargetLane},
	receive
		{reply, Return} -> Return;
		_Other			-> {error, trainer}		
	end.
	
update(TrainerPid, Data, Location) ->
	%io:format("Updating trainer ~w~n",[{Data, Location}]),
	TrainerPid ! {rec, Data, Location},
	%io:format("after Updating trainer"),
	{ok, update}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     GENERAL FUNCTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluate_target(_TargetLane, TrainerReg) ->
	%%get cars data
	_Data = filemanager:get_data_by_fullpath(TrainerReg),
	true.

write(TrainerReg, Data) ->
	%io:format("Saving to trainer log ~p ... Data ~w", [TrainerReg, Data]),
	filemanager:write_raw(TrainerReg, io_lib:format("~w", [Data])).
	
	

load_training_set() ->
	filemanager:get_data("/ann/training/training_set.txt").
	
formated_log(File) ->
	{ok, Cwd} = file:get_cwd(),
	Cwd ++ File.
	
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.
