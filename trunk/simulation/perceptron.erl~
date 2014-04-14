-module(perceptron).
-export([perceptron/6, sigmoid/1, dot_prod/2, feed_forward/3, replace_input/2, convert_to_list/1, connect_neuron/2]).
%%client interface
-export([start/1, save/3, update_connections/2, get_connections/1, init/1, reverse_connections/1, get_output/2]).

start(Args) ->
	spawn(perceptron, init, [Args]).
	
init({NeuronId,Weights,Inputs,Sensitivites, Bias, Output}) ->
	perceptron(NeuronId,Weights,Inputs,Sensitivites, Bias, Output);
	
init(NeuronId) ->
	Layer = atom_to_list(NeuronId),
	LayerType = string:substr(Layer,1,1),
	if 
		LayerType =/= "i" ->
			%Wbi = get_random_weight(),
			perceptron(NeuronId,[],[],[],{0.5, 1}, 0);
		true -> 
			perceptron(NeuronId,[],[],[],null, 0)
	end.

dot_prod([],[]) ->
	0;
dot_prod(Weights, Inputs) ->
	dot_prod(Weights, Inputs, 0).
dot_prod([] , [], Sum) ->
	Sum;
dot_prod([Wj | WTail] , [Xi | XTail], Sum) ->
	dot_prod(WTail, XTail, Sum + Wj * Xi).

%% The next dot product sum, first applies the product and after that sum all
%% VERIFICAR INEFICIENCIA YA QUE TIENE QUE RECORRER LA LISTA DOS VECES 
%% UNA PARA MULTIPLICAR Y OTRA PARA SUMAR
vector_map(_Func, [], []) ->
	[];
vector_map(Func, [Xhead | Xtail], [Yhead | Ytail]) ->
	[Func(Xhead, Yhead) | vector_map(Func, Xtail, Ytail)].
	
dot_product(X, Y) ->
	lists:foldl(fun(Element, Sum) -> Element + Sum end, 0,
		vector_map(fun(ElementX, ElementY) -> ElementX * ElementY end, X, Y)).
		
sigmoid(X) ->
	1 / (1 + math:exp(-X)).
		
%%feed_forward(Weights, Inputs) ->
%%	sigmoid(dot_product(Weights, Inputs)).

%%sigmoid_dx(X) ->
%%	math:exp(-X) / (1 + math:exp(-2 * X)).
%%feed_forward_dx(Weights, Inputs) ->
%%	sigmoid_dx(dot_product(Weights, Inputs)).
	
feed_forward(Func, Weights, Inputs) ->
	Func(dot_product(Weights, Inputs)).
	
feed_forward(Func, Weights, Inputs, WeightBias) ->
	Func(dot_product(Weights, Inputs) + WeightBias).
	
perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput) ->
	Sigmoid = 	fun(X) ->
					1 / (1 + math:exp(-X))
				end,
	Sigmoid_dx = fun(X) ->
					math:exp(-X) / (1 + math:exp(-2 * X))
				 end,
	receive
		{stimulate, Input, IsTraining} ->
			%%add input to the list
			%io:format("STIMULATE CALLED ON ~w~n", [{{LayerId, self()},Inputs, Sensitivities}]),
			%io:format("Replace input ~w on ~w for ~w~n~n", [Input, Inputs, {LayerId,self()}]),
			New_Inputs = replace_input(Inputs, Input),
			
			%%calculate output of the perceptron
			%io:format("Calculate new output on ~w for ~w~n", [{LayerId,self()}, {Sigmoid, Weights, convert_to_values(New_Inputs)}]),
			{WeightBias, _InputBias} = Bias,
			Output = feed_forward(Sigmoid, Weights, convert_to_values(New_Inputs), WeightBias),
			%io:format("New output ~w on ~w~n", [Output, {LayerId,self()}]),
			
			%%stimulate next layer
			if 	Sensitivities =/= [] ->
					%% the output is sended to at least one perceptron
					stimulate_next_layer(LayerId, convert_to_keys(Sensitivities), Output, IsTraining);					
				Sensitivities =:= [] ->
					%% there are no perceptron connected i.e output neuron
					%io:format("LEARNING ~w", [{LayerId,self()}]),
					%io:format("~nOUTPUT LAYER NEURON ~w output: ~w", [{LayerId,self()}, Output]),
					%% call learn message to self()
					%%ON TEST Get the desired output
					%network ! {desired_output, self(), LayerId},
					%receive 
					%	{reply, TargetOutput} ->
					%		io:format("~n Desired OUTPUT on ~w: ~w", [LayerId,TargetOutput])
					%		self() ! {learn, {{LayerId, self()}, TargetOutput}}						
					%end,
					
					%self() ! {learn, {{LayerId, self()}, 1}}				
					
					%%% NEW FOR NN DATE 01/12/2013
					case IsTraining of
						{true, DesiredOutputList} -> 
							%io:format("~nIs Training to output ~w to neuron ~w~n~n", [DesiredOutputList, LayerId]),
							DesiredOutput = get_desired_output(LayerId, DesiredOutputList),
														
							logger(LayerId, Output, DesiredOutput),
							
							%io:format("INPUTS B4 learing ~w~n~n", [New_Inputs]),
							self() ! {learn, {{LayerId, self()}, DesiredOutput}};					
						{false, null} -> true
					end								
			end,
			perceptron(LayerId,Weights, New_Inputs, Sensitivities, Bias, Output);
		{connect_input, SenderNeuron_PID, CallerPid} ->
			Wi = get_neuron_random_weight(),
			Wbi = get_random_weight(),
			NewInputs = [{SenderNeuron_PID, 0.5} | Inputs],			
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] ~w output connected to ~w: ~w", [?MODULE, {LayerId,self()}, SenderNeuron_PID, NewInputs])),
			CallerPid ! {init, ok},	
			perceptron(LayerId, [Wi | Weights], NewInputs, Sensitivities, {Wbi, 1}, LastOutput);
		{connect_input, SenderNeuron_PID} ->
			%Wi = get_neuron_random_weight(),
			NewInputs = [{SenderNeuron_PID, 0.5} | Inputs],
			%io:format("~w output connected to ~w: ~w~n", [{LayerId,self()}, SenderNeuron_PID, NewInputs]),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] ~w output connected to ~w: ~w", [?MODULE, {LayerId,self()}, SenderNeuron_PID, NewInputs])),			
			perceptron(LayerId, Weights, NewInputs, Sensitivities, Bias, LastOutput);
		{connecto_output, ReceiverNeuron_PID} ->
			NewOutput_PIDs = [{ReceiverNeuron_PID, 0} | Sensitivities],
			%io:format("~w output connected to ~w: ~w~n", [{LayerId,self()}, ReceiverNeuron_PID, NewOutput_PIDs]),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] ~w output connected to ~w: ~w", [?MODULE, {LayerId,self()}, ReceiverNeuron_PID, NewOutput_PIDs])),
			perceptron(LayerId, Weights, Inputs, NewOutput_PIDs, Bias, LastOutput);
		{propagate, Input, IsTraining} ->
			%io:format("Propagating input ~w ~n", [Input]),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Propagating input ~w ", [?MODULE, Input])),
			stimulate_next_layer(LayerId, convert_to_keys(Sensitivities), Input, IsTraining),
			perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput);
		{learn, Backprop} ->
			LearnRate = 0.5,
			
			WeightBias = 
			if Bias =/= null ->
				{Weight, _InputBias} = Bias,
				Weight;
			   true -> 0
			end,
			
			%%get the new sensitivites
			NewSensitivites = add_sensitivity(Sensitivities, Backprop),
			%%calculate output of the perceptron
			Output = feed_forward(Sigmoid, Weights, convert_to_values(Inputs), WeightBias),			
				
			DxValue = feed_forward(Sigmoid_dx, Weights, convert_to_values(Inputs), WeightBias),
			Sensitivity = calculate_sensitivity(Backprop, Inputs, NewSensitivites, Output, DxValue),
			
			%io:format("(~w) New Sensitivities: ~w~n", [{LayerId,self()}, NewSensitivites]),
            %io:format("(~w) Calculated Sensitivity: ~w~n", [{LayerId,self()}, Sensitivity]),
			
			%% adjust weights
			
			WeightDif = lists:map(fun(Input) -> LearnRate * Sensitivity * Input
									   end,
									   convert_to_values(Inputs)),
			NewWeights = vector_map(fun(W, D) -> W + D end, Weights, WeightDif),
			
			%%Update bias weight
			NewBias = 
				if Bias =/= null ->
					{BiasWeight, BiasInp} = Bias,
					{BiasWeight + (LearnRate * Sensitivity), BiasInp};
				   true -> Bias
				end,
			
			%io:format("(~w) Adjusted Weights: ~w~n", [{LayerId,self()}, NewWeights]),
			
			%%propagate value and weights to previous layer
			vector_map(fun(Weight, {_InputNeuronId, InputNeuron_PID}) ->
							InputNeuron_PID ! {learn,{{LayerId,self()}, Sensitivity * Weight}}
					   end,
					   NewWeights, 
					   convert_to_keys(Inputs)),
			perceptron(LayerId, NewWeights, Inputs, NewSensitivites, NewBias, Output);
		{status} ->
			io:format("Status of Node ~w ~n W: ~w~n I: ~w~n S: ~w~n",[{LayerId,self()}, Weights, Inputs, Sensitivities]),
			logger:debug_ann(loggerId, io_lib:format("[STATUS][~w] Status of Node ~w ~n W: ~w~n I: ~w~n S: ~w",[?MODULE, {LayerId,self()}, Weights, Inputs, Sensitivities])),
			perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput);
		{get_connections, CallerPid} ->
			reply(CallerPid, {Weights, Inputs, Sensitivities,Bias}),
			perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput);
		{get_id, CallerPid} ->
			reply(CallerPid, LayerId),
			perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput);
		{get_output, CallerPid} ->
			reply(CallerPid, LastOutput),
			perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput);
		{update_connections, Data} ->
			%io:format("Update incoming~n"),
			{NewWeights, NewInputs, NewSensitivities, NewBias} = Data,
			%io:format("PERCEPTRON ~w new connections ~w~n", [LayerId, {NewWeights, NewInputs, NewSensitivities}]),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] PERCEPTRON ~w new connections ~w~n", [?MODULE, LayerId, {NewWeights, NewInputs, NewSensitivities, NewBias}])),
			perceptron(LayerId, NewWeights, NewInputs, NewSensitivities, NewBias, LastOutput);
		{checkpoint, CallerPid, LogData} ->
			save_training(LayerId, Weights, Inputs, Sensitivities,Bias,LastOutput, LogData),
			reply(CallerPid, saved),
			perceptron(LayerId, Weights, Inputs, Sensitivities, Bias, LastOutput);
		{save_training, CallerPid, LogData} ->
			save_training(LayerId, Weights, Inputs, Sensitivities,Bias,LastOutput, LogData),
			reply(CallerPid, saved),
			{normal, ok};
		{reverse_connections, _CallerPid} -> %% used just to revert order of connections
			NewWeights = lists:reverse(Weights),
			NewInputs = lists:reverse(Inputs),
			NewSensitivities = lists:reverse(Sensitivities),
			perceptron(LayerId, NewWeights, NewInputs, NewSensitivities, Bias, LastOutput)
	end.
	
connect_neuron({SenderId, SenderNeuron_PID}, {ReceiverId, ReceiverNeuron_PID}) ->
	%Wi = get_neuron_random_weight(ReceiverNeuron_PID),
	SenderNeuron_PID ! {connecto_output, {ReceiverId, ReceiverNeuron_PID}},
	%ReceiverNeuron_PID ! {connect_input, {SenderId, SenderNeuron_PID}, Wi}.
	ReceiverNeuron_PID ! {connect_input, {SenderId, SenderNeuron_PID}, self()},
	receive
		{init, ok} -> true;
		Other -> io:format("ERROR CONNECTING ~w~n~n",[Other])
	end.
	
replace_input(Inputs, Input) ->
	{Input_PID, _} = Input,
	lists:keyreplace(Input_PID, 1, Inputs, Input).

convert_to_list(Inputs) ->
	lists:map(fun(Tup) ->
				{_, Val} = Tup,
				Val
			  end,
			  Inputs).

convert_to_values(Inputs) ->
	lists:map(fun(Tup) ->
				{_, Val} = Tup,
				Val
			  end,
			  Inputs).

convert_to_keys(Inputs) ->
	lists:map(fun(Tup) ->
				{Key, _} = Tup,
				Key
			  end,
			  Inputs).

stimulate_next_layer(_LayerId, [], _Output, _IsTraining) ->
	%io:format("Stimulating ENDED~n"),
	[];
stimulate_next_layer(LayerId, [{_N_id, N_Pid} | N_Tail], Input, IsTraining) ->
	%io:format("Stimulating ~w with ~w~n", [N_Pid, Input]),
	logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Stimulating ~w with ~w", [?MODULE, N_Pid, Input])),
	N_Pid ! {stimulate, {{LayerId, self()}, Input}, IsTraining},
	stimulate_next_layer(LayerId, N_Tail, Input, IsTraining).
	
add_sensitivity(Sensitivities, Backprop) when Sensitivities =/= [] ->
	replace_input(Sensitivities, Backprop);
add_sensitivity(Sensitivities, _Backprop) when Sensitivities =:= [] ->
	[].

% calculate the Sensitivities of a node
calculate_sensitivity(_Backprop, Inputs, Sensitivities, _Output, _DxValue)
	when Sensitivities =/= [], Inputs =:= [] -> %%when input node
		null;
calculate_sensitivity(Backprop, Inputs, Sensitivities, Output, DxValue)
	when Sensitivities =:= [], Inputs =/= [] -> %%when output node
		{_, TrainingValue} = Backprop,
		%io:format("Calculating sensitivites for output node. Operation (~w - ~w) * ~w~n",[TrainingValue, Output, DxValue]),		
		(TrainingValue - Output) * DxValue;
calculate_sensitivity(_Backprop, Inputs, Sensitivities, _Output, DxValue)
	when Sensitivities =/= [], Inputs =/= [] -> %%when hiden node
		DxValue * lists:foldl(fun(E, T) -> E + T end, 0, convert_to_values(Sensitivities)).

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.

save_training(LayerId, Weights, Inputs, Sensitivities,Bias,LastOutput, LogData) ->
	IdAux = atom_to_list(LayerId),
	[Tmp | _Nothing] = IdAux,
	FormatInput = format_connections_saving(Inputs),
	FormatSens = format_connections_saving(Sensitivities),
	filemanager:write_raw(LogData, io_lib:format("~w",[{LayerId, list_to_atom([Tmp]), Weights, FormatInput, FormatSens, Bias, LastOutput}])).

%%give format to inputs o sensitivities
format_connections_saving(Layer) ->
	lists:map(fun ({{Id, _Pid}, Val}) -> {Id, Val} end, Layer).

%collect([], Values) ->
%	lists:reverse(Values);
%collect([{Neuron, OldVal} | Tail], Values) ->
%	receive
%		{collect_input, CallerPid, Value} ->
%			collect(Tail, [ {Neuron, Value} | Values])
%	end.

get_desired_output(_LayerId, DesiredOutputList) when length(DesiredOutputList) == 0 ->
	30;
get_desired_output(_LayerId, DesiredOutputList) when length(DesiredOutputList) == 1 ->
	[Output | _Tail] = DesiredOutputList,
	Output;
get_desired_output(NeuronId, DesiredOutputList) ->
	NeuronIdTemp = atom_to_list(NeuronId),
	OutputIndex = string:to_integer(string:substr(NeuronIdTemp,2,length(NeuronIdTemp))),
	N = length(DesiredOutputList),
	if OutputIndex > N ->
		lists:nth(N, DesiredOutputList);
	   true ->
		lists:nth(OutputIndex, DesiredOutputList)
	end.
	
logger(NeuronId, Output, DesiredOutput) ->
	{ok, Cwd} = file:get_cwd(),
    Log = Cwd ++ "/logs/nn_train_log.txt",    
    filemanager:write_result(Log, io_lib:format("NeuronId ~w output: ~w ... Desired output: ~w",[NeuronId,Output, DesiredOutput])),
    true.
 
%%===========================================
%%===========================================
%% NYUGEN - WIDROW WEIGHT INIT
get_random_weight() ->
	Wi =
		try
			rand_generator:get_random_weight(randGen)
		catch 
			Exception:Reason -> io:format("NO RandGenerator was found. Exception ~p , Reason ~p ~n",[Exception, Reason]),
								register(randGen, rand_generator:start({})),
								rand_generator:get_random_weight(randGen)
    	end,
    Wi.
    
get_neuron_random_weight() ->
	%rand_generator:next_neuron_weight(randGen),
	rand_generator:next_neuron_weight(randGen, self()),
	receive
		{reply, Value} -> Value;
		Other -> io:format("Other perceptron: ~w", [Other]),
					error_Wi
	end.
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%client interface functions%%%%%%%%%%%

save(NeuronPid, LogPath, IsCheckpoint) ->
	case IsCheckpoint of
		false -> NeuronPid ! {save_training, self(), LogPath};
		true  -> NeuronPid ! {checkpoint, self(), LogPath}
	end,
	receive
		{reply, saved} ->
			%io:format("Save Neuron ~w~n", [NeuronPid]),
			{ok, saved};
		Other ->
			io:format("Error on neuron WAAAAAAAAAA ~w~n", [Other]),
			logger:debug_ann(loggerId, io_lib:format("[DEBUG][~w] Error on neuron WAAAAAAAAAA ~w", [?MODULE,Other])),
			{error, "bad save"}
	end.

update_connections(NeuronPid, Data) ->
	NeuronPid ! {update_connections, Data}.

get_connections(NeuronPid) ->
	NeuronPid ! {get_connections, self()},
	receive
		{reply, {Weights, Inputs, Sensitivities, Bias}} ->
			{ok, {Weights, Inputs, Sensitivities, Bias}};
		Other ->
			{error, Other}
	end.
	
reverse_connections(NeuronPid) ->
	NeuronPid ! {reverse_connections, self()},
	{ok, revert}.
	
get_output(NeuronPid, CallerPid) ->
	NeuronPid ! {get_output, CallerPid}.
	
