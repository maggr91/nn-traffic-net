%%THIS IS A DIFFERENT TRAINER USED TO TRAIN A SINGLE NN 
%%IT IS AN IMITATION OF THE MODULER module SO THE CHANGE ON SIMULATION IS MINIMUN
-module(trainer_mod).
-export([train/1, start/0, start/1, finish/0, restore/0, train/6, 
			stop/1, update_dm/3, delete_old/1, change_mode/3, get_desition/1]).

-export([init/2]).

start()->
	register(loggerId, logger:start()),
	spawn(trainer_mod, init, [{normal, trainer_ann}, standAlone]).

start(WorkMode)->
	register(loggerId, logger:start()),
	spawn(trainer_mod, init, [{normal, trainer_ann}, WorkMode]).	

restore() ->
	register(loggerId, logger:start()),
	spawn(trainer_mod, init, [{restore, trainer_ann}, standAlone]).

%init() ->
	%% When Fnot first training use the save configuration for the ann
%	TrainingData = load_training_set(),
%	register(network, ann:start()),
%	timer:apply_after(200, trainer, train, [TrainingData]).	
	
%init(Input, Hiden, Output) ->restore(
	%%train for the first time... Use new configuration
	%% When not first training use the save configuration for the ann
%	TrainingData = load_training_set(),
%	register(network, ann:start(Input, Hiden, Output)),
%	timer:apply_after(200, trainer, train, [TrainingData]).	


init({restore, TrainerID}, WorkMode) ->
	[Config | _Junk] = get_config(),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	Mode = find_config_data(Config, mode),
	
	{ModFile, NNFile} = CheckpointLog,
	
	FormatNNFile = format_dm_file(NNFile, TrainerID),	
	
	{_Struct, Mapping} = find_dm_config_data(Config, TrainerID),
	NN = restore_dm(FormatNNFile, Mapping),
	
	io:format("After restore nn~n"),
	io:format("Restore data ~p ~n",[ModFile]),
	
	RestoredData = 
		try
			restore(ModFile)
		catch 
		%exit : { noproc, _ } -> closed
			Exception:Reason -> io:format("NO TRAINER Data found. Exception ~p , Reason ~p ~n",[Exception, Reason]),
								load_training_set()
    	end,
    	
	%timer:apply_after(100, trainer_mod, update_dm, [NN, NNFile,TrainerID]),
	
	%format log files
	FormatLog = formated_log(ModFile),
		
	%delete_old([FormatLog, FormatSens]),
	timer:apply_after(100, trainer_mod,delete_old,[[FormatLog]]),
	
	NewCheckpointLog = {FormatLog, FormatNNFile},
	
	%train(TrainerID,NN, RestoredData, NewCheckpointLog, now());
	timer:sleep(600),
	train({WorkMode, TrainerID},NN, RestoredData, NewCheckpointLog, now(), Mode);

init({normal, TrainerID}, WorkMode) ->
	TrainingData = load_training_set(),
	[Config | _Junk] = get_config(),

	%NN = create_dm(Config),
	
	CheckpointLog = find_config_data(Config, checkpoint_data),
	Mode = find_config_data(Config, mode),
	
	{Other, NNFile} = CheckpointLog,
	
	FormatLog = formated_log(Other),
	FormatNNFile = format_dm_file(NNFile, TrainerID),
	
	NN = create_dm(Config,FormatNNFile, TrainerID),
	
	delete_old([FormatLog]),	
	NewCheckpointLog = {FormatLog, NNFile},
	
	timer:sleep(300),
	%update_dm(NN, NNFile, TrainerID),
		
	%train(TrainerID,NN, TrainingData, NewCheckpointLog, now()).
	io:format("Workmode ~w~n~n", [WorkMode]),
	%timer:apply_after(300, trainer_mod, listen, [{WorkMode, TrainerID},NN, TrainingData, NewCheckpointLog, now(), Mode]).
	train({WorkMode, TrainerID},NN, TrainingData, NewCheckpointLog, now(), Mode).
		
%%CREATES The decision maker for the light in this case is a Neuronal Network
%%Input: None
%%Output: Pid of the DM
create_dm(Config, NNFile, Light)->
	Exist = filelib:is_file(NNFile),
	NNConfig = find_dm_config_data(Config, Light),
	{{Input, Hidden, Output}, Mapping} = NNConfig,
	if	Exist =:= false ->
			io:format("~n~n MISSING LEARNING FILE creating new network~n~n"),
			%NNConfig = find_config_data(Config, nn_config),
			ann:start(Input, Hidden, Output, NNFile, Mapping);
		true ->
			io:format("~n~nLEARNING FILE ENCOUNTER ~p  restoring network~n~n",[NNFile]),
			restore_dm(NNFile, Mapping)
	end.

update_dm(NN, NNFile, TrainerID)->
	NewNNFile = format_dm_file(NNFile, TrainerID),
	io:format("calling update_file ~n"),
	NN ! {update_file, NewNNFile}.

restore_dm(NNFile, Mapping) ->
	%NewLight = atom_to_list(LightId) ++ ".txt",
	%NewNNFile = NNFile ++ NewLight,
	%NewNNFile = format_dm_file(NNFile, LightId),
	ann:start({NNFile, Mapping}).

train([]) ->
	timer:apply_after(1500, trainer, finish, []);

%%train([Data | TrainingData]) ->
%%	network ! {train, self(), Data},
%%	receive
%%		{reply, ok} -> train(TrainingData);
%%		{reply, ended} -> {ok, []};
%%		_Error		-> {fail, []}
%%	end.

train([Data | TrainingData]) ->
	Res = ann:train_inputs(network, Data),
	%io:format("Train network Res ~w... Data ~w~n", [Res, Data]),
	loggerId ! status,
	%logger:debug_ann(loggerId, 
	%	io_lib:format("Train network Res ~w... Data ~w", [Res, Data])),
	case Res of
		{reply, ok} -> train(TrainingData);
		{reply, ended} -> {ok, []};
		_Error		-> {fail, []}
	end.


train({standAlone, _TrainerID}, NN, [], _CheckpointLog, _BeginTime, _Mode) ->
	%checkpoint(NN, [], CheckpointLog),
	io:format("~n~nTRAINER STANDALONE~n~n"),
	timer:apply_after(8500, trainer_mod, stop, [{internal, NN}]);

train({standAlone, TrainerID}, NN, [Data | TrainingData], CheckpointLog, BeginTime, Mode) ->
	io:format("~n~nTRAINER STANDALONE~n~n"),
	CurrentTime = now(),
	case microsec_to_minutes(timer:now_diff(CurrentTime,BeginTime)) >= 20 of
		false ->
			%io:format("CONTINUING with run for data ~w.~n",[Data]),
				logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] CONTINUING with run for data ~w",[?MODULE, Data])),
			%%CONTINUE TO TRAIN
				Res = ann:input_set(NN, {Mode, Data}),
				case Res of
					{reply, ok} ->	timer:sleep(800),	
									train(TrainerID, NN,  TrainingData, CheckpointLog, BeginTime, Mode);
					_Other -> {error, unspected}
				end;
		true ->
			%io:format("SAVING CHECKPOINT.~n",[]),
				logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] SAVING CHECKPOINT", [?MODULE])),
			%write_checkpoint(NN, Data, CheckpointLog, TrainerID),
				checkpoint(NN, [], CheckpointLog),
			%%CHECKPOINT DATA
			
				train(TrainerID, NN, TrainingData, CheckpointLog, now(), Mode)
	end;
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%THIS MAIN IS USED TO EMULATE LIGHT_FSM CALLS TO MODULER

train(TrainerID, NN, TrainingData, CheckpointLog, BeginTime, Mode) ->
	receive
		{train, CallerPid} ->
			io:format("TRAIN CALLED~n~n"),
			CurrentTime = now(),
			[Data | Remaing] =  TrainingData,
			DiffTimes  = microsec_to_minutes(timer:now_diff(CurrentTime,BeginTime)),
			if DiffTimes >= 20 ->
					%io:format("SAVING CHECKPOINT.~n",[]),
					logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] SAVING CHECKPOINT", [?MODULE])),
					%write_checkpoint(NN, Data, CheckpointLog, TrainerID),
					checkpoint(NN, [], CheckpointLog);
				true -> continue
			end,
			
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] CONTINUING with run for data ~w",[?MODULE, Data])),
			%%CONTINUE TO TRAIN
			Res = ann:input_set(NN, {Mode, Data}),
			case Res of
				{reply, Value} ->	
						  reply(CallerPid, {Mode, Value}),
						  train(TrainerID, NN, Remaing, CheckpointLog, now(), Mode);
								%timer:sleep(800),	
								%train(TrainerID, NN,  TrainingData, CheckpointLog, BeginTime, Mode);
				Other -> reply(CallerPid, {error, Other}),
			     		  train(TrainerID, NN, TrainingData, CheckpointLog, now(), Mode)
			end;
		{update_mode, NewMode, CallerPid} ->
			reply(CallerPid, ok),
			train(TrainerID, NN, TrainingData, CheckpointLog, now(), NewMode);
		{status, CallerPid} ->
			io:format("Test~n~n"),
			reply(CallerPid, test),
			train(TrainerID, NN, TrainingData, CheckpointLog, now(), Mode);
		stop -> 
			stop({internal, NN})
	end.
%train(TrainerID, NN, TrainingData, CheckpointLog, BeginTime, Mode) ->
%	io:format("~n~nTRAINER PROCESS ~w~n~n",[{TrainerID, NN, TrainingData, CheckpointLog, BeginTime, Mode}]),
%	receive
%		{train, CallerPid} ->
%			io:format("TRAIN CALLED~n~n"),
%			CurrentTime = now(),
%			[Data | Remaing] =  TrainingData,
%			DiffTimes  = microsec_to_minutes(timer:now_diff(CurrentTime,BeginTime)),
%			if DiffTimes >= 20 ->
%					%io:format("SAVING CHECKPOINT.~n",[]),
%					logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] SAVING CHECKPOINT", [?MODULE])),
%					%write_checkpoint(NN, Data, CheckpointLog, TrainerID),
%					checkpoint(NN, [], CheckpointLog)	
%			end,
%			
%			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] CONTINUING with run for data ~w",[?MODULE, Data])),
			%%CONTINUE TO TRAIN
%			Res = ann:input_set(NN, {Mode, Data}),
%			case Res of
%				{reply, Value} ->	
%						  reply(CallerPid, {Mode, Value}),
%						  train(TrainerID, NN, Remaing, CheckpointLog, now(), Mode);
								%timer:sleep(800),	
								%train(TrainerID, NN,  TrainingData, CheckpointLog, BeginTime, Mode);
%				_Other -> reply(CallerPid, {error, unspected}),
%			     		  train(TrainerID, NN, TrainingData, CheckpointLog, now(), Mode)
%			end;
						
%		{update_mode, NewMode, CallerPid} ->
%			reply(CallerPid, ok),
%			train(TrainerID, NN, TrainingData, CheckpointLog, now(), NewMode);
%		{status, CallerPid} ->
%			io:format("Test~n~n"),
%			reply(CallerPid, test),
%			train(TrainerID, NN, TrainingData, CheckpointLog, now(), Mode);
%		stop ->
%			timer:apply_after(8500, trainer_mod, stop, [{internal, NN}])
%	end.	

finish() ->
	io:format("===========================================~n~n Ending training ~n~n===========================================~n~n"),
	Res = ann:stop(network),
	case Res of
		{ok, []} ->
			io:format("Training finished please check test_saving.txt~n~n"),
			{normal, ok};
		_Other ->
			{error, []}
	end.
		
load_training_set() ->
	filemanager:get_data("/training/training_set.txt").

%%find configuration
find_config_data(ConfigData, Key) ->
	Res = lists:keyfind(Key, 1, ConfigData),
	case Res of
		false -> [];
		{Key, Value} -> Value
	end.

get_config() ->
	filemanager:get_data("/config_mod.txt").	

stop({internal, NN}) ->
	force_stop(NN);
stop(TrainerPid) ->
	TrainerPid ! stop,
	{normal, trainer_mod}.

force_stop(NN) ->
	NN ! {stop, self()}.

delete_old(List) ->
	lists:map(fun(File) -> file:delete(File) end, List). %% delete old log

formated_log(File) ->
	{ok, Cwd} = file:get_cwd(),
	Cwd ++ File.

format_dm_file(NNFile, LightId) ->
	{ok, Cwd} = file:get_cwd(),
	NewLight = atom_to_list(LightId) ++ ".txt",
	FileAux = Cwd ++ NNFile,
	FileAux ++ NewLight.
	
microsec_to_minutes(Value) ->
	((((Value / 1000) / 1000) / 60)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHECKPOINT
%%%%%%%%%%%%%
checkpoint(NN, Data, CheckpointLog) ->
	{ModulerFile, _NNFile} = CheckpointLog,
	filemanager:write_raw(ModulerFile, io_lib:format("~w", [Data])),
	NN ! {checkpoint, self()}.
	

%write_checkpoint(NN, Data, CheckpointLog, TrainerID) ->
%	{TrainerFile, _NNFile} = CheckpointLog,
	%io:format("moduler file: ~p", [ModulerFile]),
%	filemanager:write_raw(TrainerFile, io_lib:format("~w", [{TrainerID, Data}])),
%	write_checkpoint_nn(NN).

%write_checkpoint_nn(NN) ->
%	NN ! {checkpoint, self()}.
	
%%Uses the default file load in the Process to restore the data
restore(File) ->
	filemanager:get_data(File).

find_element(_Id, []) ->
	[];
find_element(Id, Data) ->
	Element = lists:keyfind(Id, 1, Data),
	case Element of
		false ->	[];
		_Other ->	{Id, Value} = Element,
					Value
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
	
	
reply (Pid, Reply) ->
    Pid ! {reply, Reply}.
    
    
change_mode(TrainerPid, CallerPid, Mode) ->
 	TrainerPid ! {update_mode, Mode, CallerPid}.
 	
get_desition(TrainerPid) ->
	io:format("~nTrainerPID ~w~n",[TrainerPid]),
	TrainerPid ! {train, self()},
	receive
		{reply, Value} -> Value;
		Other -> Other
	end.
