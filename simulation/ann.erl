-module(ann).
-export([start/1, start/3, start/5, start/6, test_run/0, input_set/2, stop/1, get_data/1]).

-export([init/1, init/3, init/5, init/6]).

start(Args)->	
	on_init(),
	%timer:sleep(100),
	%perceptron:set_random_seed(),	
	%nguyen_widrow_random_weights(1, "initWeights.txt"),
	spawn(ann, init, [Args]).
	
start(Input, Hiden, Output) ->
	on_init(),
	%timer:sleep(100),
	%perceptron:set_random_seed(),	
	%nguyen_widrow_random_weights(1, "initWeights.txt"),
	spawn(ann, init, [Input, Hiden, Output]).
	
start(Input, Hiden, Output, NNFile, OutputsMapping) ->
	on_init(),
	spawn(ann, init, [Input, Hiden, Output, NNFile, OutputsMapping]).
	
start(Input, Hiden, Output, NNFile, OutputsMapping, NNLog) ->
	on_init(),
	spawn(ann, init, [Input, Hiden, Output, NNFile, OutputsMapping, NNLog]).

init({NNFile, OutputsMapping}) ->
	%% When training use the save configuration for the ann was found
	Layers = load_layers(NNFile),
	NNLog = default_log(),
	network(Layers, NNFile, OutputsMapping, NNLog);
init({NNFile, OutputsMapping, NNLog}) ->
	%% When training use the save configuration for the ann was found
	Layers = load_layers(NNFile),
	network(Layers, NNFile, OutputsMapping, NNLog);
init(NNFile) ->
	%% When training use the save configuration for the ann was found
	Layers = load_layers(NNFile),
	OutputsMapping = [],
	NNLog = default_log(),
	network(Layers, NNFile, OutputsMapping, NNLog).


%%when loading a network from old process
load_layers(NNFile) ->
	{ConfigX, ConfigH, ConfigY} = check_for_learning(NNFile),
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
	lists:map(fun({Id, _Type, Weights,Inputs,Sensitivites, Bias, Output}) -> 
					%{Id, spawn(perceptron, perceptron, [Id,Weights,Inputs,Sensitivites,{0.5, 1}])} 
					{Id, perceptron:start({Id,Weights,Inputs,Sensitivites,Bias, Output})}
			  end, 
			  LayerItems
			 ).
	
custom_load_layer(HiddenLayerItems) ->
	lists:map(fun({Id, _Type, Weights,Inputs,Sensitivites, Bias, Output}) -> 
				%{Id, spawn(perceptron, perceptron, [Id,Weights,Inputs,Sensitivites,{0.5, 1}])} 
				{Id, perceptron:start({Id,Weights,Inputs,Sensitivites,Bias, Output})}
			  end, 
			  HiddenLayerItems).

update_connections(InputLay, HiddenLay, OutputLay)->
	%io:format("Update connections Data ~w~n", [{InputLay, HiddenLay, OutputLay}]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Update connections Data ~w", [?MODULE, {InputLay, HiddenLay, OutputLay}])),
	%io:format("Update connections for input~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Update connections for input",[?MODULE])),
	update_connections_aux(InputLay, [], HiddenLay),
	%io:format("Update connections for hidden~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Update connections for hidden",[?MODULE])),
	update_connections_aux(HiddenLay,InputLay, OutputLay),
	%io:format("Update connections for output~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Update connections for output", [?MODULE])),
	update_connections_aux(OutputLay,HiddenLay, []).
	
update_connections_aux([], _LayerA, _LayerB) ->
	{ok, []};
update_connections_aux([{_NeuronId, NeuronPid} | NeuronsTail], LayerInput, LayerSensitivities) ->
	Res = perceptron:get_connections(NeuronPid),
	case Res of
		{ok, {Weights, Inputs, Sensitivities, Bias}} ->	
			%io:format("PERCEPTRON CONNECTIONS: ~w  ",[{Weights, Inputs, Sensitivities}]),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] PERCEPTRON CONNECTIONS: ~w  ",[?MODULE, {Weights, Inputs, Sensitivities, Bias}])),
			NewInputs = get_connections_pids(Inputs, LayerInput),
			NewSensitivities = get_connections_pids(Sensitivities, LayerSensitivities),
			perceptron:update_connections(NeuronPid, {Weights, NewInputs, NewSensitivities, Bias}),
			update_connections_aux(NeuronsTail, LayerInput, LayerSensitivities);
		{error, Msg} ->
			{error, Msg}			
	end.
	
%update_connections_aux([], _LayerA, _LayerB) ->
%	{ok, []};
%update_connections_aux([{_NeuronId, NeuronPid} | NeuronsTail], LayerInput, LayerSensitivities) ->
%	NeuronPid ! {get_connections, self()},
%	receive
%		{reply, {Weights, Inputs, Sensitivities}} ->
			%io:format("Old connections ~w~n", [{{Weights, Inputs, Sensitivities}}]),
%			NewInputs = get_connections_pids(Inputs, LayerInput),
			%io:format("NEW inputs: ~w~n", [NewInputs]),
%			NewSensitivities = get_connections_pids(Sensitivities, LayerSensitivities),
			%io:format("NEW Sensitivities: ~w~n", [NewSensitivities]),
%			NeuronPid ! {update_connections, {Weights, NewInputs, NewSensitivities}},
%			update_connections_aux(NeuronsTail, LayerInput, LayerSensitivities)
			%%set_new_connections({NeuronId, NeuronPid},Weights, NewInputs, NewSensitivities)
%	end.

get_connections_pids([], _Source) ->
	[];
get_connections_pids(Target, Source) ->
	%io:format("GET CONNECTIONS PIDS ANN TARGET: ~w, SOURCES: ~w~n", [Target, Source]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] GET CONNECTIONS PIDS ANN TARGET: ~w, SOURCES: ~w~n", [?MODULE, Target, Source])),
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
	OutputsMapping = [],
	NNLog = default_log(),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] New network", [?MODULE])),
	Layers = load_layers(Input, Hiden, Output),
	network(Layers, "test_saving.txt", OutputsMapping, NNLog).
	
init(Input, Hiden, Output, NNFile, OutputsMapping) ->
	io:format("New network~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] New network", [?MODULE])),
	Layers = load_layers(Input, Hiden, Output),
	NNLog = default_log(),
	network(Layers, NNFile, OutputsMapping, NNLog).
	
init(Input, Hiden, Output, NNFile, OutputsMapping, NNLog) ->
	io:format("New network~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] New network", [?MODULE])),
	Layers = load_layers(Input, Hiden, Output),
	network(Layers, NNFile, OutputsMapping, NNLog).

%% creates a new network from scratch
load_layers(Input, Hiden, Output) ->
	InputLay = layer_from_scratch({i, Input}),
	HiddenLay = layer_from_scratch({h, Hiden}),
	OutputLay = layer_from_scratch({o, Output}),
	io:format("Input: ~w~n Hidden: ~w~n Output: ~w~n", [InputLay, HiddenLay, OutputLay]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Input: ~w~n Hidden: ~w~n Output: ~w~n", [?MODULE, InputLay, HiddenLay, OutputLay])),
	connect_all({InputLay, HiddenLay, OutputLay}),

	%%on a separate method, initialize weights
	%init_weights({InputLay, HiddenLay, OutputLay}),
	
	%%set init weights values for the NN using NGUYEN-WIDROW
	nguyen_widrow(InputLay, HiddenLay, OutputLay),
	
	%[{input, lists:reverse(InputLay)},{hidden, lists:reverse(HiddenLay)},{output, lists:reverse(OutputLay)}].
	[{input, InputLay},{hidden, HiddenLay},{output, OutputLay}].

%% When loading a network from 0
layer_from_scratch(LayerConfig) ->
	layer_from_scratch(LayerConfig,1, []).	
layer_from_scratch({Type, Max},Index, Layer) when Index =< Max->
	NeuronId = list_to_atom(lists:flatten(io_lib:format("~w~w",[Type, Index]))),
	NeuronPid = perceptron:start(NeuronId),
	%NeuronPid = spawn(perceptron, perceptron, [NeuronId,[],[],[],{0.5, 1}]),
	layer_from_scratch({Type, Max},Index + 1, [{NeuronId, NeuronPid} | Layer]);
layer_from_scratch(_LayerConfig, _Index, Layer) ->
	lists:reverse(Layer).

%%create connection between the new layers
connect_all({Input, Hidden, Output}) ->
	%io:format("Connecting... Input: ~w~n  with Hidden: ~w~n", [Input, Hidden]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Connecting... Input: ~w~n  with Hidden: ~w", [?MODULE, Input, Hidden])),
	connect_layers(Input, Hidden),
	%io:format("Connecting... Hidden: ~w~n  with Output: ~w~n", [Hidden,Output]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Connecting... Hidden: ~w~n  with Output: ~w~n", [?MODULE, Hidden,Output])),
	connect_layers(Hidden, Output), 
	timer:sleep(200),
	fix_connections({Input, Hidden, Output}).

connect_layers(_SenderLayer, []) ->
	{ok, []};
connect_layers(SenderLayer, [ReceiverNeuron | ReceiverTail]) ->
	lists:map(fun(SenderNeuron) ->
				perceptron:connect_neuron(SenderNeuron,ReceiverNeuron)
			  end,
			  SenderLayer
			 ),
	connect_layers(SenderLayer, ReceiverTail).
				
%% just reverse every connection for the neuron
fix_connections({Input, Hidden, Output}) ->
	FullList = lists:append([Input, Hidden, Output]),
	lists:map(fun({_NeuronId, NeuronPid}) ->
				perceptron:reverse_connections(NeuronPid)
			  end,
			  FullList
			 ).
			 
			 
init_weights({InputLay, HiddenLay, OutputLay}) ->
	FullList = lists:append([HiddenLay, OutputLay]),
	lists:map(fun({_NeuronId, NeuronPid}) ->
				perceptron:init_weights(NeuronPid)
			  end,
			  FullList
			 ).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
network(Layers, NNFile, OutputsMapping, NNLog) ->	
	receive
		{train, CallerPid, Data} ->
			try
					io:format("SENDING INPUTS~n~n"),
					logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] SENDING INPUTS\n", [?MODULE])),
					Res = send_input(Layers,Data, OutputsMapping, NNLog),
					reply(CallerPid, Res)
			catch
					Exception:Reason -> io:format("Error sending training data Exception ~p , Reason ~p ~n",[Exception, Reason]),
										logger:debug_ann(loggerId, io_lib:format("[ERROR][~w] SENDING INPUTS ~p\n", [?MODULE, Reason])),
										reply(errorNet, CallerPid, "A neuron failed to receive the data")
			end,			
			network(Layers, NNFile, OutputsMapping, NNLog);
		{stop, _CallerPid} ->
			filelib:ensure_dir("checkpoint/nn_saving/"),
			try					
					save(Layers, NNFile, false),
					%reply(CallerPid, ended),
					stop_helpers(),
					kill_neurons(Layers)
			catch
					Exception:Reason -> io:format("Error stopping training data Exception ~p , Reason ~p ~n",[Exception, Reason]),
										logger:debug_ann(loggerId, io_lib:format("[ERROR][~w] SENDING INPUTS ~p\n", [?MODULE, Reason]))
										%reply(errorNet, CallerPid, Reason)
			end,
			{normal, ok};
		{checkpoint, CallerPid} ->
			filelib:ensure_dir("checkpoint/nn_saving/"),
			try
					save(Layers, NNFile, true)
					%reply(CallerPid, ended),
			catch
					Exception:Reason -> io:format("Error saving training data Exception ~p , Reason ~p ~n",[Exception, Reason]),
										logger:debug_ann(loggerId, io_lib:format("[ERROR][~w] SENDING INPUTS ~p\n", [?MODULE, Reason]))
										%reply(errorNet, CallerPid, Reason)
			end,
			network(Layers, NNFile, OutputsMapping, NNLog);
		{get_data, CallerPid} ->
			reply(CallerPid, {OutputsMapping, NNLog}),
			network(Layers, NNFile, OutputsMapping, NNLog);
		{update_file, NewNNFile} ->
			io:format("Updating NNFILe"),
			network(Layers, NewNNFile, OutputsMapping, NNLog)
	end.			

%% set a new training set for the network
send_input(Layers, {Mode, Data}, OutputsMapping, NNLog) ->
	{_InType, InputLayer} = lists:keyfind(input, 1,Layers),	
	{_Train, TrainInput} = lists:keyfind(inputs, 1,Data),	
	
	io:format("Sending inputs according mode ~w . Data ~w~n", [Mode, Data]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Sending inputs according mode ~w . Data ~w", [?MODULE, Mode, Data])),
	Res = case Mode of
		normal ->
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Normal output call not learning", [?MODULE])),
			{_OutType, OutputLayer} = lists:keyfind(output, 1,Layers),
			set_input_aux(InputLayer, TrainInput, {false, null}),
			%%Retrieve outputs from the output neurons
			timer:sleep(200),
			R = get_neurons_outputs(OutputLayer, OutputsMapping),
			io:format("~n~n Outneurons ~w~n~n", [R]),
			filemanager:write_raw(NNLog, io_lib:format("[~w, ~w]", [{inputs, TrainInput}, {outputs, R}])),
			R;
		_Other ->	%%training		
			{_DesairedOut, TargetOutput} = lists:keyfind(output, 1,Data),			
			set_input_aux(InputLayer, TrainInput, {true, TargetOutput}),
			[]
	end,
	io:format("FINISH Sending inputs~n~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] FINISH Sending inputs\n", [?MODULE])),
	Res.

%set_input(first, InputLayer, TrainInput, TargetOutput, _OutputLayer)->
%	set_input_aux(InputLayer, TrainInput, {true, TargetOutput}).
	%%set_input(wait, InputLayer, TrainInput, TargetOutput, OutputLayer);
%set_input(wait, InputLayer, TrainInput, TargetOutput, OutputLayer)->
%	receive
%		{desired_output, CallerPid, CallerId} ->
%			io:format("~n~n~n~nCall from neuron: ~w to get Desired output: ~w ~n~n",[CallerId,TargetOutput]),
%			{Last, _Pid} = lists:last(OutputLayer), %string:to_integer([lists:last(L)])
%			io:format("Call from ~w, output ~w... Last: ~w~n",[CallerId, TargetOutput,Last]),
%			reply(CallerPid, TargetOutput),
%			if Last =/= CallerId ->
%					io:format("Waiting for next output call~n"),
%					set_input(wait, InputLayer, TrainInput, TargetOutput, OutputLayer);
%			   Last =:= CallerId ->
%			   		io:format("Last output call~n"),
%			   		{normal, ok}
%			end			
%	end.

%set_input(first, InputLayer, TrainInput, _OutputLayer)->
%	set_input_aux(InputLayer, TrainInput, {false, null}).

set_input_aux([], [], _TargetOutput) ->
	io:format("All inputs passed to neurons ~n~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] All inputs passed to neurons", [?MODULE])),
	{ok, []};
set_input_aux([], _RInputs, _TargetOutput) ->
	{ok, missingneurons};
set_input_aux([{NeuronId, NeuronPid} | NeuronTail], [Val | InputTail], TargetOutput) ->
	io:format("Neuron: ~w pass val: ~w",[NeuronId,Val]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Neuron: ~w pass val: ~w",[?MODULE, NeuronId,Val])),
	NeuronPid ! {propagate, Val, TargetOutput},
	set_input_aux(NeuronTail,InputTail, TargetOutput).

%%INPUT: OutputLayer list of neurons from the output layer
%%		 OutputsMapping determines the begining and end of custom outputs [1, N[
%%OUTPUT: List of all outputs formated
%%DESC: From each perceptron get its output value and combine any bits to represent decimal values
get_neurons_outputs(OutputLayer, OutputsMapping) ->
	%%GET all outputs
	io:format("~n~n OutputLayer: ~w OutputsMapping: ~w~n~n",[OutputLayer,OutputsMapping]),
	L = lists:map(fun({_NeuronId, NeuronPid}) ->
						perceptron:get_output(NeuronPid, self()),
						receive
							{replyNeuron, Value} -> round(Value);
							{errorNeuron, _Msg}   -> error
						end
					end,
					OutputLayer
				),
	io:format("~n Outneurons bits ~w~n~n", [L]),
	get_output_mapping(L, OutputsMapping).
	
get_output_mapping(List, OutputsMapping) ->
	lists:map(fun({_Id, {Start, Len}}) ->
				L = lists:sublist(List, Start, Len),
				list_to_integer(lists:foldl(fun(Item, Complete) ->  Complete ++ integer_to_list(Item) end, "", L), 2)
				end,
				OutputsMapping
	).

save(Layers, NNFile, IsCheck) ->
	%io:format("Saving training of layers ~w~n", [Layers]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Saving training of layers ~w~n", [?MODULE, Layers])),
	file:delete(NNFile),
	%io:format("Old file deleted~n"),			
	save_training(Layers, NNFile, IsCheck).
	
save_training([], _LogPath, _IsCheck) ->
	{ok,saved};
save_training([{Type, Layer} | LayersTail], LogPath, IsCheck) ->
	%io:format("Saving Layer ~w: ~w on ~p~n", [Type, Layer, LogPath]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Saving Layer ~w: ~w on ~p~n", [?MODULE, Type, Layer, LogPath])),
	save_training_aux(Layer, LogPath, IsCheck),
	save_training(LayersTail, LogPath, IsCheck).
	
%%save_training(Layers, LogPath) ->
%%	io:format("Saving Layer: ~w on ~p~n", [Layers, LogPath]),
%%	Layer = lists:foldl(fun({_Type, List}, Main) -> lists:append(Main, List) end, [], Layers),
%%	save_training_aux(Layer, LogPath),
%%	{ok, saved}.

save_training_aux([], _LogPath, _IsCheck) ->
	%io:format("No more neurons to save~n"),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] No more neurons to save", [?MODULE])),
	{ok, []};
save_training_aux([{NeuronId, NeuronPid} | NeuronTail], LogPath, IsCheck) ->
	%io:format("Saving neuron ~w: ~w on ~p~n", [NeuronId, NeuronPid, LogPath]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Saving neuron ~w: ~w on ~p", [?MODULE, NeuronId, NeuronPid, LogPath])),
	Res = perceptron:save(NeuronPid, LogPath, IsCheck),
	%io:format("Saving neuron result ~w. Next ~w~n", [Res, NeuronTail]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Saving neuron result ~w. Next ~w", [?MODULE, Res, NeuronTail])),
	case Res of
		{ok, saved} ->
			%io:format("Reply save succesful, continue~n"),
			save_training_aux(NeuronTail, LogPath, IsCheck);
		{error, Msg} ->
			io:format("ERROR!!!!!!~n"),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] ERROR!!!!!! ~p", [?MODULE, Msg])),
			{error, "bad save"}
	end.
	
test_run()->
	true.

reply(Pid, Reply) ->
    Pid ! {replyNet, Reply}.
    
reply(ReplyKey, Pid, Reply) ->
    Pid ! {ReplyKey, Reply}.
    
%load_structure() ->
%	filemanager:get_data("structure.txt").
check_for_learning(NNFile) ->
	%Data = filemanager:get_data(NNFile),
	Data = filemanager:get_data_by_fullpath(NNFile),
	Input = filter_neurons(i, Data),
	Hidden = filter_neurons(h, Data),
	Output = filter_neurons(o, Data),	
	{Input, Hidden, Output}.
filter_neurons(Filter, Data) ->
	lists:filter(fun({_Id, Type, _,_,_,_,_}) -> Type =:= Filter end, Data).

display_status(Layer) ->
	lists:map(fun({_Id, Pid}) -> Pid ! {status} end, Layer).

%%===========================================
%%===========================================
%% NGUYEN - WIDROW WEIGHT INIT
nguyen_widrow(Input, Hidden, Output) ->
	%% get count of hidden nodes
	H = length(Hidden),
	I = length(Input),
	
	io:format("#H = ~w, #inputs ~w ~n",[H, I]),
	Beta = nguyen_widrow_beta(H, I),	
	io:format("Beta = ~w~n",[Beta]),
	
	%%Loading weights for bias
	rand_generator:gen_biases(randGen, Beta),
	
	HLayerWeights = get_layer_weights(Hidden),
	OLayerWeights = get_layer_weights(Output),
	
	%io:format("Hidden weights ~w~n~n",[HLayerWeights]),	
	%io:format("Ouput weights ~w~n~n",[OLayerWeights]),
	
	HNorm = euclidean_norm(HLayerWeights),
	ONorm = euclidean_norm(OLayerWeights),
	
	nguyen_widrow_adjust_weights(Hidden, Beta, HNorm),
	nguyen_widrow_adjust_weights(Output, Beta, ONorm),
	io:format("Finished adjusting weights TOTAL~n").

nguyen_widrow_beta(H, I) ->
	0.7 * math:pow(H, 1/I).
	
euclidean_norm(LayerWeights) ->
	math:sqrt(lists:foldl(fun(Wi, Sum) -> Sum + math:pow(Wi, 2) end, 0, LayerWeights)).


nguyen_widrow_adjust_weights(Layer, Beta, Norm) ->
	lists:map(fun({_NeuronId, NeuronPid}) ->
		%%Call rand_generator and get a new bias weight based on the beta value
		NewBias = rand_generator:next_bias_weight(randGen),
		
		{ok, {Weights, Inputs, Sensitivities, _Bias}} = perceptron:get_connections(NeuronPid),
		NewWeights = nguyen_widrow_adjust_weights_aux(Weights, Beta, Norm),
		perceptron:update_connections(NeuronPid, {NewWeights, Inputs, Sensitivities, {NewBias, 1}}) 
		end,
		Layer		
	),
	io:format("Finished adjusting weights for layer~n").


%%Updates weights for each neuron on the layer
%%INPUT: Weights neuron weights
%%OUTPUT Adjusted weights using NGUYEN-WIDROW Algorithm
nguyen_widrow_adjust_weights_aux(Weights, Beta, Norm) ->
	lists:map(fun(Wi) -> (Beta * Wi) / Norm end, Weights).

get_layer_weights(Layer) ->
	Temp = lists:reverse(
		lists:foldl(fun({_NeuronId, NeuronPid}, List) ->
			{ok, {Weights, _Inputs, _Sensitivities, _Bias}} = perceptron:get_connections(NeuronPid),
			%io:format("~n~n~n~nANN PID ~w~n~n", [self()]),
			%NeuronPid ! {get_connections, self()},
			%receive
			%	{reply, {Weights, _Inputs, _Sensitivities, _Bias}} ->
			%		[Weights | List];
			%	Other ->
			%		io:format("ERROR ~w~n~n", [Other]),
			%		[error | List ]
			%end
			[Weights | List] end, [], Layer)
	),
	
	lists:append(Temp).	

on_init() ->
	filelib:ensure_dir("logs/"),
	filelib:ensure_dir("logs/nn/"),
	safe_helper_init(loggerId, logger, null, logger),
	safe_helper_init(randGen, randGen, {}, rand_generator).
	
safe_helper_init(RegName, LogName, Args, MODULE) ->
	IsReg = whereis(RegName),
	io:format("~n~n ~w REGISTER ~w~n", [LogName, IsReg]),
	try
		case IsReg of 
		   undefined when Args == null ->		
			register(RegName, MODULE:start());
           undefined when Args /= null ->
			register(RegName, MODULE:start(Args));
	   	   _Other ->
	   		ok
		end
	catch 		
			Exception:Reason -> io:format("~w already registered, continue. Exception ~p , Reason ~p ~n",[LogName, Exception, Reason]),
								continue
    end.
	
stop_helpers() ->
	logger:stop(loggerId),
	rand_generator:stop(randGen).
	
default_log() ->
	{ok, Cwd} = file:get_cwd(),
	Name = default_log_name() ++ ".txt",	
	Cwd ++ Name.

default_log_name() ->
	Temp = tuple_to_list(calendar:local_time()),
	DTL = lists:foldl(fun(Item, Acc) -> ItemList = tuple_to_list(Item), lists:append(Acc, ItemList) end, [], Temp),
	lists:foldl(fun(Item, Acc) -> Str = Acc ++ "_", Str ++ lists:flatten(io_lib:format("~p", [Item])) end, "", DTL).
	
	
kill_neurons(Layers) ->
	true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%client interface functions%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_set(NetworkPid, InputSet) ->
	io:format("Train network ~w... Data ~w~n", [NetworkPid, InputSet]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Train network ~w... Data ~w~n", [?MODULE, NetworkPid, InputSet])),
	NetworkPid ! {train, self(), InputSet},
	receive
		{replyNet, Res} -> {reply, Res};
		{errorNet, Msg}		-> {fail, Msg}
	end.
	
stop(NetworkPid) ->
	NetworkPid ! {stop, self()},
	receive
		{replyNet, ended} ->			
			{ok, []};
		_Other ->
			{error, []}
	end.
	
get_data(NetworkPid) ->
	NetworkPid ! {get_data, self()},
	receive 
		{replyNet, Data} -> Data;
		{errorNet, _Msg} -> 	{error, no_data}
	end.
