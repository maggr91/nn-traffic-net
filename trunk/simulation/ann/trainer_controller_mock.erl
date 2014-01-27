-module(trainer_controller_mock).

-export([start/0, change_mode/2, execute/1]).

-export([init/0]).

start() ->
	spawn(trainer_controller_mock, init, []).
	

init() ->
	Trainer_Mod_PID = trainer_mod:start(controlled),
	io:format("TRAINER MOD PID ~w~n~n", [Trainer_Mod_PID]),
	listen(Trainer_Mod_PID).
	
	
listen(Trainer_Mod_PID) ->
	receive
		{desition, CallerPid} ->
			Desition = trainer_mod:get_desition(Trainer_Mod_PID),
			io:format("Desition Result ~w ~n",[Desition]),
			reply(CallerPid, Desition),
			listen(Trainer_Mod_PID);
		{change_mode, CallerPid, Mode} ->
			trainer_mod:change_mode(Trainer_Mod_PID, self(), Mode),
			receive
				{reply, ok} -> io:format("Mode Cambiado~n");
				_Other  -> true
			
			end,			
			reply(CallerPid, ok),
			listen(Trainer_Mod_PID);
		stop ->
			trainer_mod:stop(Trainer_Mod_PID)
	end.
	
	
reply (Pid, Reply) ->
    Pid ! {reply, Reply}.
    
change_mode(MockPid, NewDMMode) ->
	MockPid ! {change_mode, self(), NewDMMode},
	receive
		{reply, Msg} -> Msg;
		Other ->		Other	
	end.
	
execute(MockPid) ->
	MockPid ! {desition, self()},
	receive
		{reply, Msg} -> Msg;
		Other -> Other
	end.

