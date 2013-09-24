-module(trainer).
-export([train/1, start/0, start/3, finish/0]).

-export([init/0, init/3]).

start()->
	spawn(trainer, init, []).
	
start(Input, Hiden, Output) ->
	spawn(trainer, init, [Input, Hiden, Output]).

init() ->
	%% When not first training use the save configuration for the ann
	TrainingData = load_training_set(),
	register(network, ann:start()),
	timer:apply_after(200, trainer, train, [TrainingData]).	
	
init(Input, Hiden, Output) ->
	%%train for the first time... Use new configuration
	%% When not first training use the save configuration for the ann
	TrainingData = load_training_set(),
	register(network, ann:start(Input, Hiden, Output)),
	timer:apply_after(200, trainer, train, [TrainingData]).	

train([]) ->
	timer:apply_after(700, trainer, finish, []);
train([Data | TrainingData]) ->
	network ! {train, self(), Data},
	receive
		{reply, ok} -> train(TrainingData);
		{reply, ended} -> {ok, []};
		_Error		-> {fail, []}
	end.	

finish() ->
	io:format("===========================================~n~n Ending training ~n~n===========================================~n~n"),
	network ! {stop, self()},
	receive
		{reply, ended} ->
			io:format("Training finished please check test_saving.txt~n~n"),
			{ok, []};
		_Other ->
			{error, []}
	end.
		
load_training_set() ->
	filemanager:get_data("/training/training_set.txt").
