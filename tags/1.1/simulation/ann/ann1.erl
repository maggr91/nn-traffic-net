-module(ann).
-export([perceptron/4, sigmoid/1, dot_prod/2, feed_forward/3, replace_input/2, convert_to_list/1, connect_neuron/2, test_run/0]).

test_run() ->
	I1_pid = spawn(ann, perceptron, [i1,[],[],[]]),
	I2_pid = spawn(ann, perceptron, [i2,[],[],[]]),
	I3_pid = spawn(ann, perceptron, [i3,[],[],[]]),
	
	H1_pid = spawn(ann, perceptron, [h1,[],[],[]]),
	H2_pid = spawn(ann, perceptron, [h2,[],[],[]]),
	H3_pid = spawn(ann, perceptron, [h3,[],[],[]]),
	
	O1_pid = spawn(ann, perceptron, [o1,[],[],[]]),
	O2_pid = spawn(ann, perceptron, [o2,[],[],[]]),
	O3_pid = spawn(ann, perceptron, [o3,[],[],[]]),
	
	io:format("CONNECTIONS~n"),
	%connect input layer to hidden layer
	ann:connect_neuron(I1_pid,H1_pid),
	ann:connect_neuron(I2_pid,H1_pid),
	ann:connect_neuron(I3_pid,H1_pid),
	
	ann:connect_neuron(I1_pid, H2_pid),
	ann:connect_neuron(I2_pid, H2_pid),
	ann:connect_neuron(I3_pid, H2_pid),
	
	ann:connect_neuron(I1_pid, H3_pid),
	ann:connect_neuron(I2_pid, H3_pid),
	ann:connect_neuron(I3_pid, H3_pid),
	
	%%connect hiden with output
	ann:connect_neuron(H1_pid, O1_pid),
	ann:connect_neuron(H2_pid, O1_pid),
	ann:connect_neuron(H3_pid, O1_pid),
	
	ann:connect_neuron(H1_pid, O2_pid),
	ann:connect_neuron(H2_pid, O2_pid),
	ann:connect_neuron(H3_pid, O2_pid),
	
	ann:connect_neuron(H1_pid, O3_pid),
	ann:connect_neuron(H2_pid, O3_pid),
	ann:connect_neuron(H3_pid, O3_pid),
	
	io:format("STATUS ~n"),
	I1_pid ! {status},
	I2_pid ! {status},
	I3_pid ! {status},
	
	H1_pid ! {status},
	H2_pid ! {status},
	H3_pid ! {status},
	
	O1_pid ! {status},
	O1_pid ! {status},
	O1_pid ! {status},
	
	io:format("PROPAGATE~n"),
	I1_pid ! {propagate, 1.8},
	%I2_pid ! {propagate, 2},
	%I3_pid ! {propagate, 10},
	
	I1_pid ! {status},
	I2_pid ! {status},
	I3_pid ! {status},
	
	H1_pid ! {status},
	H2_pid ! {status},
	H3_pid ! {status},
	
	O1_pid ! {status},
	O1_pid ! {status},
	O1_pid ! {status}.

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
vector_map(Func, X, Y) ->
	%%io:format("VECTOR MAP PARAM FUNCTION: ~p~n", [{Func, X,Y}]),
	vector_map(Func, X, Y, []).
vector_map(_Func, [], [], Map) ->
	Map;
vector_map(Func, [Xhead | Xtail], [Yhead | Ytail], Map) ->
	vector_map(Func, Xtail, Ytail, [Func(Xhead, Yhead) | Map]).
	
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
	
perceptron(LayerId, Weights, Inputs, Sensitivities) ->
	Sigmoid = 	fun(X) ->
					1 / (1 + math:exp(-X))
				end,
	Sigmoid_dx = fun(X) ->
					math:exp(-X) / (1 + math:exp(-2 * X))
				 end,
	receive
		{stimulate, Input} ->
			%%add input to the list
			io:format("STIMULATE CALLED ON ~w~n", [{{LayerId, self()},Inputs, Sensitivities}]),
			io:format("Replace input ~w on ~w for ~w~n~n", [Input, Inputs, {LayerId,self()}]),
			New_Inputs = replace_input(Inputs, Input),
			
			%%calculate output of the perceptron
			io:format("Calculate new output on ~w for ~w~n", [{LayerId,self()}, {Sigmoid, Weights, convert_to_values(New_Inputs)}]),
			Output = feed_forward(Sigmoid, Weights, convert_to_values(New_Inputs)),
			io:format("New output ~w on ~w~n", [Output, {LayerId,self()}]),
			
			%%stimulate next layer
			if 	Sensitivities =/= [] ->
					%% the output is sended to at least one perceptron
					stimulate_next_layer(LayerId, convert_to_keys(Sensitivities), Output);
				Sensitivities =:= [] ->
					%% there are no perceptron connected i.e output neuron
					io:format("LEARNING ~w", [{LayerId,self()}]),
					io:format("~n~w outputs: ~w", [{LayerId,self()}, Output]),
					%% call learn message to self()
					self() ! {learn, {self(), 1}}
			end,
			perceptron(LayerId,Weights, New_Inputs, Sensitivities);
		{connect_input, SenderNeuron_PID} ->
			NewInputs = [{SenderNeuron_PID, 0.5} | Inputs],
			io:format("~w output connected to ~w: ~w~n", [{LayerId,self()}, SenderNeuron_PID, NewInputs]),
			perceptron(LayerId, [0.5 | Weights], NewInputs, Sensitivities);
		{connecto_output, ReceiverNeuron_PID} ->
			NewOutput_PIDs = [{ReceiverNeuron_PID, 0} | Sensitivities],
			io:format("~w output connected to ~w: ~w~n", [{LayerId,self()}, ReceiverNeuron_PID, NewOutput_PIDs]),
			perceptron(LayerId, Weights, Inputs, NewOutput_PIDs);
		{propagate, Input} ->
			io:format("Propagating input ~w ~n", [Input]),
			stimulate_next_layer(LayerId, convert_to_keys(Sensitivities), Input);
		{learn, Backprop} ->
			LearnRate = 0.5,
			
			%%get the new sensitivites
			NewSensitivites = add_sensitivity(Sensitivities, Backprop),
			%%calculate output of the perceptron
			Output = feed_forward(Sigmoid, Weights, convert_to_values(Inputs)),			
			DxValue = feed_forward(Sigmoid_dx, Weights, convert_to_values(Inputs)),
			Sensitivity = calculate_sensitivity(Backprop, Inputs, NewSensitivites, Output, DxValue),
			
			io:format("(~w) New Sensitivities: ~w~n", [{LayerId,self()}, NewSensitivites]),
            io:format("(~w) Calculated Sensitivity: ~w~n", [{LayerId,self()}, Sensitivity]),
			
			%% adjust weights
			
			WeightDif = lists:map(fun(Input) -> LearnRate * Sensitivity * Input
									   end,
									   convert_to_values(Inputs)),
			NewWeights = vector_map(fun(W, D) -> W + D end, Weights, WeightDif),
			io:format("(~w) Adjusted Weights: ~w~n", [{LayerId,self()}, NewWeights]),
			
			%%propagate value and weights to previous layer
			vector_map(fun(Weight, InputNeuron_PID) ->
							InputNeuron_PID ! {learn,{self(), Sensitivity * Weight}}
					   end,
					   NewWeights, 
					   convert_to_keys(Inputs)),
			perceptron(LayerId, NewWeights, Inputs, NewSensitivites);
		{status} ->
			io:format("Status of Node ~w ~n W: ~w~n I: ~w~n S: ~w~n",[{LayerId,self()}, Weights, Inputs, Sensitivities]),
			perceptron(LayerId, Weights, Inputs, Sensitivities)
	end.
	
connect_neuron(SenderNeuron_PID, ReceiverNeuron_PID) ->
	SenderNeuron_PID ! {connecto_output, ReceiverNeuron_PID},
	ReceiverNeuron_PID ! {connect_input, SenderNeuron_PID}.
	
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

stimulate_next_layer(_LayerId, [], _Output) ->
	io:format("Stimulating ENDED~n"),
	[];
stimulate_next_layer(LayerId, [N_Pid | N_Tail], Input) ->
	io:format("Stimulating ~w with ~w~n", [N_Pid, Input]),
	N_Pid ! {stimulate, {self(), Input}},
	stimulate_next_layer(LayerId, N_Tail, Input).
	
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
		(TrainingValue - Output) * DxValue;
calculate_sensitivity(_Backprop, Inputs, Sensitivities, _Output, DxValue)
	when Sensitivities =/= [], Inputs =/= [] -> %%when hiden node
		DxValue * lists:foldl(fun(E, T) -> E + T end, 0, convert_to_values(Sensitivities)).
