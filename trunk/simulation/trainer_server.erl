-module(trainer_server).
-export([start/0, update/3, update/4]).

-export([init/0]).

start()->
	spawn(trainer_server, init, []).
	
init() ->
	%%start the trainer server (Used by all lanes to register cars statistics) in the network
	%%load the training sets
	filelib:ensure_dir("logs/training/"),
	TrainerReg = formated_log("/logs/training/training_rec_"),	
	io:format("Training server ONLINE~n",[]),
	train(TrainerReg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%         MAIN           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train(TrainerReg) ->
	receive
		{rec, Data, _Location} ->
			write(TrainerReg, Data),
			train(TrainerReg);
		{rec_ind, Data,_Location, ParentLaneId} ->
			io:format("Rec called with parentLane"),
			write(TrainerReg, Data, ParentLaneId),
			train(TrainerReg);
		killyou ->
		    {normal, ok}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     END  MAIN          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update(TrainerPid, Data, Location) ->
	%io:format("Updating trainer ~w~n",[{Data, Location}]),
	TrainerPid ! {rec, Data, Location},
	%io:format("after Updating trainer"),
	{ok, update}.

update(TrainerPid, Data, Location, ParentLaneId) ->
	io:format("Using ParentLaneId ~w trainer ~w~n",[ParentLaneId, {Data, Location}]),
	TrainerPid ! {rec_ind, Data,Location, ParentLaneId},
	io:format("after Updating trainer"),
	{ok, update}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     GENERAL FUNCTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write(TrainerReg, Data) ->
	%io:format("Saving to trainer log ~p ... Data ~w", [TrainerReg, Data]),
	filemanager:write_raw(TrainerReg, io_lib:format("~w", [Data])).
	
write(TrainerReg, Data, ParentLaneId) ->
	Extension = atom_to_list(ParentLaneId) ++ ".txt",
	FinalRegFile = TrainerReg ++ Extension,
	%io:format("Saving to trainer log ~p ... Data ~w", [TrainerReg, Data]),
	filemanager:write_raw(FinalRegFile, io_lib:format("~w", [Data])).

formated_log(File) ->
	{ok, Cwd} = file:get_cwd(),
	Cwd ++ File.
