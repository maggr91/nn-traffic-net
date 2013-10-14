-module(moduler).
-export([start/0,start/1, test/0, connect/3, status_test/0, update_from_nn/2, request_updates/1]).
-export([init/0, init/1]).

%% Used to comunicate traffic lights 
%% this acts like a buffer for the communication
%% stores a default value first, so if it's called by any other traffic light
%% it will answer with the value that it already has this to avoid extra
%% load to the lights, an so they focus to use the network an simulate

start() ->
	spawn(moduler, init, []).

start(Args) ->
	spawn(moduler, init, [Args]).

init() ->
	[Config | _Junk] = get_config(),
	NN = create_dm(Config),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	listen({},[{av, []}, {ca,[]}], NN,[{siblings_data, []},{net_values, []}]).
	
init({Light, NN}) ->
	[Config | _Junk] = get_config(),
	CheckpointLog = find_config_data(Config, checkpoint_data),
	listen(Light,[{av, []}, {ca,[]}],NN, [{siblings_data, []},{net_values, []}], CheckpointLog).
	
%%CREATES The decision maker for the light in this case is a Neuronal Network
%%Input: None
%%Output: Pid of the DM
create_dm(Config) ->
	
	NNConfig = find_config_data(Config, nn_config),
	{Input, Hidden, Output} = NNConfig,
	ann:start(Input, Hidden, Output).

listen(Light, Siblings, NN, Data, CheckpointLog) ->
	receive
		%%call siblings
		{broadcast_request, _CallerPid} -> %%Ask siblings to send an update
			io:format("Data before broadcast: ~w", [Data]),
			NewData = request_update_from_Siblings(Siblings, Data),
			listen(Light, Siblings, NN, NewData);
		{broadcast_update, CallerPid, NewData} -> %%Send siblings to an update
			io:format("Update triggered by siblings ~w Light: ~w... NewData", [CallerPid,Light]),
			UpdateData = update_siblings_data(CallerPid, Data, NewData),
			listen(Light, Siblings, NN, UpdateData);
		{response, CallerPid} ->
			{_Key, ResponseData} = lists:keyfind(net_values,1,Data),
			reply(CallerPid, ResponseData),
			listen(Light, Siblings, NN, Data);
			%% send info to caller
		{update_nn, CallerPid, NewVals} ->
			NewData = update_nn_data(Data, NewVals),
			reply(CallerPid, ok),
			send_update_to_Siblings(Siblings, NewVals),
			listen(Light, Siblings, NN, NewData);
		{connect, SiblingPid, Dir} ->
			{Dir, List} = lists:keyfind(Dir, 1, Siblings),
			NewSinbligs = lists:keyreplace(Dir, 1, Siblings, {Dir, [SiblingPid | List]}),
			listen(Light, NewSinbligs, NN, Data);
		{status, _CallerPid} ->
			io:format("Status of moduler ~w ~n Siblings: ~w~n Data: ~w~n~n",[{Light,self()}, Siblings, Data]),
			listen(Light, Siblings, NN, Data)
	end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

connect(Dir, SenderPid, ReceiverPid) ->
	SenderPid ! {connect, ReceiverPid, Dir},
	ReceiverPid ! {connect, SenderPid, Dir}.

request_update_from_Siblings([], Data) ->
	io:format("Data after broadcast: ~w~n", [Data]),
	Data;    
request_update_from_Siblings([{Dir, SiblingsList} | Siblings], Data) ->
	io:format("Updating ~w~n", [Dir]),
	NewData = update_from_Siblings_aux(SiblingsList, Data),
	request_update_from_Siblings(Siblings, NewData).
	
update_from_Siblings_aux([], Data) ->
	Data;
update_from_Siblings_aux([S | Siblings], Data) ->
	S ! {response, self()},
	receive
		{reply, NewData} ->
			Res = lists:keyfind(S,1, Data),
			case Res of
				false ->	update_from_Siblings_aux(Siblings, [{S, NewData} | Data]); 	
				_Other -> 	UpdatedData = lists:keyreplace(S,1,Data,{S, NewData}),
							update_from_Siblings_aux(Siblings, UpdatedData)
			end;						
		_Other ->
			{error, badarg}			
	end.


send_update_to_Siblings(Siblings, Data) ->
	lists:map(
		fun({Dir, SiblingsList}) ->
			io:format("Sending update to siblings on ~w. List: ~w~n~n", [Dir, SiblingsList]),
			lists:map(
				fun(SiblingPid) ->
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

update_nn_data(Data, NewVals) ->
	{net_values, _Values} = lists:keyfind(net_values, 1, Data),
	lists:keyreplace(net_values, 1, Data, {net_values, NewVals}).


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

status_test() ->
	L = [m1,m2,m3,m4,m5,m6,m7,m8,m9],
	lists:map(fun (Mod) -> Mod ! {status, self()} end, L),
	{ok, []}.
	
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