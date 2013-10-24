-module(moduler).
-export([start/0,start/1, test/0, connect/3,status/1, status_test/0, update_from_nn/2, request_updates/1, checkpoint/1, stop/1]).
-export([init/1, update_dm/3]).

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
	{ModFile, NNFile, SensorFile} = CheckpointLog,
	
	NN = restore_dm(NNFile, LightId),
	RestoredData = restore(ModFile, LightId),	
	
	Sensor = restore_sens(SensorFile,Lanes, LightId),	
	
	timer:apply_after(100, moduler, update_dm, [NN, NNFile, LightId]),
	listen(LightId,[{av, []}, {ca,[]}], NN,RestoredData,CheckpointLog, Sensor);

init({normal, LightId, Lanes}) ->
	[Config | _Junk] = get_config(),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	{Other, NNFile, SensorFile} = CheckpointLog,
	
	FormatLog = formated_log(Other),
	FormatSens = format_dm_file(SensorFile, LightId),
	
	NewCheckpointLog = {FormatLog, NNFile, FormatSens},
	NN = create_dm(Config),
	update_dm(NN, NNFile, LightId),	
	
	Sensor = create_sens(Lanes, FormatSens),
	
	listen(LightId,[{av, []}, {ca,[]}], NN,[{siblings_data, []},{net_values, []}, {net_input, []}],NewCheckpointLog, Sensor).
		
%%CREATES The decision maker for the light in this case is a Neuronal Network
%%Input: None
%%Output: Pid of the DM
create_dm(Config) ->
	NNConfig = find_config_data(Config, nn_config),
	{Input, Hidden, Output} = NNConfig,
	ann:start(Input, Hidden, Output).

update_dm(NN, NNFile, LightId)->
	NewNNFile = format_dm_file(NNFile, LightId),
	NN ! {update_file, NewNNFile}.

restore_dm(NNFile, LightId) ->
	NewLight = atom_to_list(LightId) ++ ".txt",
	NewNNFile = NNFile ++ NewLight,
	ann:start(NewNNFile).

%%%%%%%%%%%
%% SENSOR

create_sens(Lanes, FormatSens) ->
	sensor:start({normal, {Lanes, FormatSens}}).
	
restore_sens(SensorFile,Lanes, LightId) ->
	NewLight = atom_to_list(LightId) ++ ".txt",
	NewSensFile = SensorFile ++ NewLight,
	Pid = sensor:start({restore, {Lanes, NewSensFile}}),
	Pid ! {update_file, NewSensFile},
	Pid.

format_dm_file(NNFile, LightId) ->
	{ok, Cwd} = file:get_cwd(),
	NewLight = atom_to_list(LightId) ++ ".txt",
	FileAux = Cwd ++ NNFile,
	FileAux ++ NewLight.
	
formated_log(File) ->
	{ok, Cwd} = file:get_cwd(),
	Cwd ++ File.
	
listen(Light, Siblings, NN, Data, CheckpointLog, Sensor) ->
	receive
		%%call siblings
		{broadcast_request, _CallerPid} -> %%Ask siblings to send an update
			io:format("Data before broadcast: ~w", [Data]),
			NewData = request_update_from_Siblings(Siblings, Data),
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor);
		{broadcast_update, CallerPid, NewData} -> %%Send siblings to an update
			io:format("Update triggered by siblings ~w Light: ~w... NewData", [CallerPid,Light]),
			UpdateData = update_siblings_data(CallerPid, Data, NewData),
			listen(Light, Siblings, NN, UpdateData, CheckpointLog, Sensor);
		{response, CallerPid} ->
			{_Key, ResponseData} = lists:keyfind(net_values,1,Data),
			reply(CallerPid, ResponseData),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor);
			%% send info to caller
		{update_nn, CallerPid, NewVals} ->
			NewData = update_nn_data(net_values, Data, NewVals),
			reply(CallerPid, ok),
			send_update_to_Siblings(Siblings, NewVals),
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor);
		{inputs, _CallerPid, NewVals} ->
			NewData = update_nn_data(net_input, Data, NewVals),			
			listen(Light, Siblings, NN, NewData, CheckpointLog, Sensor);
		{connect, SiblingData, Dir} ->
			{Dir, List} = lists:keyfind(Dir, 1, Siblings),
			Exist = lists:any(fun(Item) -> Item == SiblingData end, List),
			case Exist of
				false -> NewSinbligs = lists:keyreplace(Dir, 1, Siblings, {Dir, [SiblingData | List]}),
						 listen(Light, NewSinbligs, NN, Data, CheckpointLog, Sensor);
				true  -> listen(Light, Siblings, NN, Data, CheckpointLog, Sensor)
			end;
		{proc, CallerPid, Dir} ->
			FormatedInputs = format_inputs(Dir, Data),
			
			%%call nn with the new inputs
			%ann:input(FormatedInputs)
			
			Desition = true,
			reply(CallerPid, Desition), 
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor);
			
		{stop, CallerPid} ->
			force_stop(NN, Sensor),
			reply(CallerPid, ok),
			{normal, moduler};
		{checkpoint, CallerPid} ->
			write_checkpoint(NN, Data, CheckpointLog, Light, Sensor),
			reply(CallerPid, ok),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor);			
		{status, _CallerPid} ->
			io:format("Status of moduler ~w ~n Siblings: ~w~n Data: ~w~n~n",[{Light,self()}, Siblings, Data]),
			listen(Light, Siblings, NN, Data, CheckpointLog, Sensor)
	end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

connect(Dir, {SenderId, SenderPid}, {ReceiverId, ReceiverPid}) ->
	SenderPid ! {connect, {ReceiverId, ReceiverPid}, Dir},
	ReceiverPid ! {connect, {SenderId, SenderPid}, Dir}.
	
%connect(Dir, SenderPid, ReceiverPid) ->
%	SenderPid ! {connect, ReceiverPid, Dir}.

request_update_from_Siblings([], Data) ->
	io:format("Data after broadcast: ~w~n", [Data]),
	Data;    
request_update_from_Siblings([{Dir, SiblingsList} | Siblings], Data) ->
	io:format("Updating ~w~n", [Dir]),
	NewData = update_from_Siblings_aux(SiblingsList, Data),
	request_update_from_Siblings(Siblings, NewData).
	
update_from_Siblings_aux([], Data) ->
	Data;
update_from_Siblings_aux([{Sid, S} | Siblings], Data) ->
	S ! {response, self()},
	receive
		{reply, NewData} ->
			UpdatedData = update_siblings_data(Sid, Data, NewData),
			update_from_Siblings_aux(Siblings, UpdatedData);
			%Res = lists:keyfind(S,1, Data),
			%case Res of
			%	false ->	update_from_Siblings_aux(Siblings, [{S, NewData} | Data]); 	
			%	_Other -> 	UpdatedData = lists:keyreplace(S,1,Data,{S, NewData}),
			%				update_from_Siblings_aux(Siblings, UpdatedData)
			%end;						
		_Other ->
			{error, badarg}			
	end.


send_update_to_Siblings(Siblings, Data) ->
	lists:map(
		fun({Dir, SiblingsList}) ->
			io:format("Sending update to siblings on ~w. List: ~w~n~n", [Dir, SiblingsList]),
			lists:map(
				fun({_SiblingId, SiblingPid}) ->
					SiblingPid ! {broadcast_update, self(), Data}
				end,
				SiblingsList
			)
		end,
		Siblings
	).								


update_siblings_data(Sibling, Data, NewData) ->
	{_Key, SiblingList} = lists:keyfind(siblings_data,1, Data),
	Item = lists:keyfind(Sibling,1, SiblingList),
	case Item of
		false ->
			io:format("No data founded, adding info for sibling"),
			NewSiblingData = [{Sibling,NewData} | SiblingList];
		{Sibling, _Value} ->
			io:format("Previous data founded, updating for sibling"),
			NewSiblingData = lists:keyreplace(Sibling, 1, SiblingList, {Sibling, NewData})
	end,
	lists:keyreplace(siblings_data, 1, Data, {siblings_data, NewSiblingData}).

update_nn_data(Id, Data, NewVals) ->
	%{net_values, _Values} = lists:keyfind(net_values, 1, Data),
	%lists:keyreplace(net_values, 1, Data, {net_values, NewVals}).	
	lists:keyreplace(Id, 1, Data, {Id, NewVals}).


%%find configuration
find_config_data(ConfigData, Key) ->
	Res = lists:keyfind(Key, 1, ConfigData),
	case Res of
		false -> [];
		{Key, Value} -> Value
	end.

get_config() ->
	filemanager:get_data("/config_mod.txt").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     CLIENT INTERFACE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Update ann data

update_from_nn(ModulerPid, Values) ->
	ModulerPid ! {update_nn, self(), Values},
	receive
		{reply, ok} ->
			{ok, udpated};
		_Other ->
			io:format("error"),
			{error, []}
	end.
	
request_updates(ModulerPid) ->
	ModulerPid ! {broadcast_request, self()},
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

calculate_outputs(NetValues, _NetInputs) ->
	PromCars = random:uniform(5),
	ExtraOutputs = [PromCars],
	NewValues = lists:append(NetValues, ExtraOutputs),
	NewValues.
	
format_inputs(_Dir, _Data) ->
	{}.
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
	
find_element(Id, Data) ->
	{Id, Element} = lists:keyfind(Id, 1, Data),
	case Element of
		false ->	[];
		_Other ->	Element
	end.

force_stop(NN, Sensor) ->
	NN ! {stop, self()},
	Sensor ! {stop, self()}.
	
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