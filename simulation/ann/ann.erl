-module(ann).
-export([start/0, start/3, test_run/0]).

-export([init/0, init/3]).

start()->
	spawn(ann, init, []).
	
start(Input, Hiden, Output) ->
	spawn(ann, init, [Input, Hiden, Output]).

init() ->
	%% When not first training use the save configuration for the ann
	Layers = load_layers(),
	network(Layers).

%%when loading a network from old process
load_layers() ->
	{ConfigX, ConfigH, ConfigY} = check_for_learning(),
	InputLay = load_layer(ConfigX),
	HiddenLay = custom_load_layer(ConfigH),
	OutputLay = load_layer(ConfigY),
	io:format("Input: ~w~n Hidden: ~w~n Output: ~w~n", [InputLay, HiddenLay, OutputLay]),
	update_connections(InputLay, HiddenLay, OutputLay),
	display_status(InputLay),
	display_status(HiddenLay),
	display_status(OutputLay),	
	[{input, InputLay},{hidden, HiddenLay},{output, OutputLay}].

load_layer(LayerItems) ->
	lists:map(fun({Id, _Type, Weights,Inputs,Sensitivites}) -> 
					{Id, spawn(perceptron, perceptron, [Id,Weights,Inputs,Sensitivites])} 
			  end, 
			  LayerItems
			 ).
	
custom_load_layer(HiddenLayerItems) ->
	lists:map(fun({Id, _Type, Weights,Inputs,Sensitivites}) -> 
				{Id, spawn(perceptron, perceptron, [Id,Weights,Inputs,Sensitivites])} 
			  end, 
			  HiddenLayerItems).

update_connections(InputLay, HiddenLay, OutputLay)->
	io:format("Update connections for input~n"),
	update_connections_aux(InputLay, [], HiddenLay),
	io:format("Update connections for hidden~n"),
	update_connections_aux(HiddenLay,InputLay, OutputLay),
	io:format("Update connections for output~n"),
	update_connections_aux(OutputLay,HiddenLay, []).
	
update_connections_aux([], _LayerA, _LayerB) ->
	{ok, []};
update_connections_aux([{_NeuronId, NeuronPid} | NeuronsTail], LayerInput, LayerSensitivities) ->
	NeuronPid ! {get_connections, self()},
	receive
		{reply, {Weights, Inputs, Sensitivities}} ->
			%io:format("Old connections ~w~n", [{{Weights, Inputs, Sensitivities}}]),
			NewInputs = get_connections_pids(Inputs, LayerInput),
			%io:format("NEW inputs: ~w~n", [NewInputs]),
			NewSensitivities = get_connections_pids(Sensitivities, LayerSensitivities),
			%io:format("NEW Sensitivities: ~w~n", [NewSensitivities]),
			NeuronPid ! {update_connections, {Weights, NewInputs, NewSensitivities}},
			update_connections_aux(NeuronsTail, LayerInput, LayerSensitivities)
			%%set_new_connections({NeuronId, NeuronPid},Weights, NewInputs, NewSensitivities)
	end.

get_connections_pids(Target, Source) ->
	lists:map(fun({NeuronId, Pid}) -> 
				Neuron = lists:keyfind(NeuronId, 1, Target),
				{NeuronId, Val} =  Neuron,
				{{NeuronId, Pid}, Val}
			  end,
			  Source
			 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%train for the first time... Use new configuration
init(Input, Hiden, Output) ->
	io:format("New network~n"),
	Layers = load_layers(Input, Hiden, Output),
	network(Layers).

%% creates a new network from scratch
load_layers(Input, Hiden, Output) ->
	InputLay = layer_from_scratch({i, Input}),
	HiddenLay = layer_from_scratch({h, Hiden}),
	OutputLay = layer_from_scratch({o, Output}),
	io:format("Input: ~w~n Hidden: ~w~n Output: ~w~n", [InputLay, HiddenLay, OutputLay]),
	connect_all({InputLay, HiddenLay, OutputLay}),
	[{input, lists:reverse(InputLay)},{hidden, lists:reverse(HiddenLay)},{output, lists:reverse(OutputLay)}].

%% When loading a network from 0
layer_from_scratch(LayerConfig) ->
	layer_from_scratch(LayerConfig,1, []).	
layer_from_scratch({Type, Max},Index, Layer) when Index =< Max->
	NeuronId = list_to_atom(lists:flatten(io_lib:format("~w~w",[Type, Index]))),
	NeuronPid = spawn(perceptron, perceptron, [NeuronId,[],[],[]]),
	layer_from_scratch({Type, Max},Index + 1, [{NeuronId, NeuronPid} | Layer]);
layer_from_scratch(_LayerConfig, _Index, Layer) ->
	Layer.

%%create connection between the new layers
connect_all({Input, Hidden, Output}) ->
	io:format("Connecting... Input: ~w~n  with Hidden: ~w~n", [Input, Hidden]),
	connect_layers(Input, Hidden),
	io:format("Connecting... Hidden: ~w~n  with Output: ~w~n", [Hidden,Output]),
	connect_layers(Hidden, Output).

connect_layers(_SenderLayer, []) ->
	{ok, []};
connect_layers(SenderLayer, [ReceiverNeuron | ReceiverTail]) ->
	lists:map(fun(SenderNeuron) ->
				perceptron:connect_neuron(SenderNeuron,ReceiverNeuron)
			  end,
			  SenderLayer
			 ),
	connect_layers(SenderLayer, ReceiverTail).
				


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
network(Layers) ->	
% 	io:format("Saving training~n"),
%	file:delete("test_saving.txt"),
%	io:format("Old file deleted~n"),			
%	save_training(Layers, "test_saving.txt"),
	%%reply(CallerPid, ended),
%	{normal, ok}.
	receive
		{train, CallerPid, Data} ->
			set_input(Layers,Data),
			reply(CallerPid, ok),
			network(Layers);
		{stop, CallerPid} ->
			io:format("Saving training~n"),
			file:delete("test_saving.txt"),
			io:format("Old file deleted~n"),			
			save_training(Layers, "test_saving.txt"),
			reply(CallerPid, ended),
			{normal, ok}
	end.			

%% set a new training set for the network
set_input(Layers, Data) ->
	{_Type, InputLayer} = lists:keyfind(input, 1,Layers),
	{_Train, TrainInput} = lists:keyfind(input, 1,Data),
	{_DesairedOut, _DesairedOutput} = lists:keyfind(output, 1,Data),
	io:format("Neuron: ~w pass val: ~w",[InputLayer,TrainInput]),
	set_input_aux(InputLayer, TrainInput),
	true.

set_input_aux([], []) ->
	{ok, []};
set_input_aux([{NeuronId, NeuronPid} | NeuronTail], [Val | InputTail]) ->
	io:format("Neuron: ~w pass val: ~w",[NeuronId,Val]),
	NeuronPid ! {propagate, Val},
	set_input_aux(NeuronTail,InputTail).
	
save_training([], _LogPath) ->
	{ok,saved};
save_training([{Type, Layer} | LayersTail], LogPath) ->
	io:format("Saving Layer ~w: ~w on ~p~n", [Type, Layer, LogPath]),
	save_training_aux(Layer, LogPath),
	save_training(LayersTail, LogPath).
	
save_training_aux([], _LogPath) ->
	{ok, []};
save_training_aux([{NeuronId, NeuronPid} | NeuronTail], LogPath) ->
	io:format("Saving neuron ~w: ~w on ~p~n", [NeuronId, NeuronPid, LogPath]),
	NeuronPid ! {save_training, self(), LogPath},
	receive
		{reply, saved} ->
			io:format("Reply save succesful, continue~n"),
			save_training_aux(NeuronTail, LogPath);
		_Other ->
			{error, "bad save"}
	end.
	
test_run()->
	true.

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.
    
%load_structure() ->
%	filemanager:get_data("structure.txt").
check_for_learning() ->
	Data = filemanager:get_data("/test_saving.txt"),
	Input = filter_neurons(i, Data),
	Hidden = filter_neurons(h, Data),
	Output = filter_neurons(o, Data),	
	{Input, Hidden, Output}.
filter_neurons(Filter, Data) ->
	lists:filter(fun({_Id, Type, _,_,_}) -> Type =:= Filter end, Data).

display_status(Layer) ->
	lists:map(fun({_Id, Pid}) -> Pid ! {status} end, Layer).
