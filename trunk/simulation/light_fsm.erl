-module(light_fsm).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         get_state/1]).

%states
-export([ redred/2, redred/3, greenred/2, greenred/3, redgreen/2, redgreen/3]).
%-record(state,{}).

%client calls
-export([start_link/1,move_avenue/1, move_street/1, idle/1,update_siblings/1, evaluate_state/1, 
			tabulate_data/1, checkpoint_data/1, restore/1, change_times/1]).

%test
-export([test/0]).

%%client functions

init_move_avenue(Pid) ->
    gen_fsm:sync_send_event(Pid,init_move_avenue).
    
init_move_street(Pid) ->
    gen_fsm:sync_send_event(Pid,init_move_street).


move_avenue(Pid) ->
    gen_fsm:sync_send_event(Pid,move_avenue).
    
move_street(Pid) ->
    gen_fsm:sync_send_event(Pid,move_street).

idle(Pid) ->
    gen_fsm:sync_send_event(Pid,idle).

update_siblings({Pid, Siblings}) ->
    gen_fsm:sync_send_event(Pid,{update_siblings, Siblings}).

tabulate_data({Pid, DataLog}) ->
    gen_fsm:sync_send_event(Pid,{tabulate_data, DataLog}).
 
checkpoint_data({Pid, DataLog}) ->
    gen_fsm:sync_send_event(Pid,{checkpoint, DataLog}).

restore({Pid, RestoredData}) ->
    gen_fsm:sync_send_event(Pid,{restore, RestoredData}).
    
change_times({Pid, NewTimes}) ->
    gen_fsm:sync_send_event(Pid,{change_times, NewTimes}).
    
%% gen_fsm functions

start_link(Args) ->
    gen_fsm:start_link(?MODULE,Args,[]).

init(Args) ->
    %%Mode indicates if normal or a restore from checkpoints
    {Mode, LightId, ManagedLanes,Siblings, Times, LogData} = Args,
    file:delete(LogData), %% delete old log
    NewTimes = [{allred_timer, 0} | Times],
    
    CtrlMod = moduler:start({Mode, LightId, ManagedLanes}),
    %{ok, redred,{LightId, ManagedLanes,Siblings, NewTimes, LogData, redred, CtrlMod}}.
    StateData = [{id,LightId}, {managed_lanes, ManagedLanes},{siblings, Siblings}, 
    	{times, NewTimes}, {log_data, LogData}, {old_state, redred}, {ctrl_mod, CtrlMod}],
    
    case Mode of
    	normal -> scan_lanes(StateData);
    	_Other -> continue
    end,
    
    {ok, redred, StateData}.

get_state(LightPid) ->
    try
        %io:fwrite("Getting state ~w~n", [LightPid]),
        gen_fsm:sync_send_event(LightPid, get_state)
    catch 
	exit : { noproc, _ } -> closed
    end.

handle_event(shutdown, _StateName, StateData) ->
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    io:fwrite("gen_fsm called ~w:handle_event(~w, ~w, ~w)~n",
        [?MODULE, Event, StateName, StateData]),
    {next_state, StateName, StateData}.

handle_sync_event(get_state, _From, StateName, State) ->
    {reply, { StateName, State }, StateName, State };
handle_sync_event(Event, From, StateName, StateData) ->
    io:fwrite("gen_fsm called ~w:handle_sync_event(~w, ~w, ~w, ~w)~n",
        [?MODULE, Event, From, StateName, StateData]),
    {next_state, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
    io:fwrite("gen_fsm called ~w:handle_info(~w, ~w, ~w)~n",
        [?MODULE,Info,StateName,StateData]),
    {next_state, StateName, StateData}.
    
terminate(Reason, StateName, StateData) ->
    io:fwrite("gen_fsm called ~w:terminate(~w, ~w, ~w)~n",
        [?MODULE, Reason, StateName, StateData]).

code_change(OldVsn, StateName, StateData, Extra) ->
    io:fwrite("gen_fsm called ~w:code_change(~w, ~w, ~w, ~w)~n",
        [?MODULE, OldVsn, StateName, StateData, Extra]),
    {ok, StateName, StateData}.
    
%% IDLE STATE
redred(move_avenue, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId, ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewStateData = process_state(allred_move, StateData, [], [av, ca], null, avenue),
    {next_state, greenred, NewStateData};
redred(move_street, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),    
    %{LightId, ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewStateData = process_state(allred_move, StateData, [], [ca, av], null, street),
    {next_state, redgreen, NewStateData};
redred(Event, StateData) ->
    unexpected_event(redred, Event, StateData),
    {next_state, redred, StateData}.

%%Synch calls
redred(get_state, _From, StateData) ->
	ReturnData = get_state_data(redred, StateData),
    {reply, ReturnData,redred, StateData};
redred({update_siblings, Siblings},_From, StateData) ->
    %io:format("Updating StateData: ~w~n",[Siblings]),
    %%{LightId,ManagedLanes,_OldSiblings, Times, LogData, OldState, CtrlMod} = StateData,
    
    %%CREATE THE MODULER HERE USING THE AV OR DIR
    %%SPAWN THE NN TOO AND ADD IT TO THE MODULER
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod}    
    update_moduler(StateData, Siblings),
    NewStateData = update_state_data([{siblings, Siblings}], StateData),    
    {reply, {redred,Siblings},redred, NewStateData};
    
redred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %%{_LightId,ManagedLanes, _Siblings, _Times, _LogData, _OldState, _CtrlMod} = StateData,
    ManagedLanes = find_element(managed_lanes, StateData),
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {redred,DataLog},redred, StateData};
redred({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, redred, OldState}, CtrlMod),
    write_checkpoint(redred, DataLog, StateData),
    {reply, {redred,DataLog}, redred, StateData};
redred({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,    
    %LightId = find_element(id, StateData),
    %io:format("Restore for: ~p~n",[LightId]),    
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {redred,RestoredData}, RestoredState, NewStateData};
redred({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data(NewTimes, StateData),
    {reply, {redred,NewTimes}, redred, NewStateData};
redred(move_avenue,_From, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av, ca],CTime, 0, LogData),
    %NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    NewStateData = process_state(allred_move, StateData, [], [av, ca], null, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redred,Siblings},greenred, NewStateData};
redred(move_street,_From, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca, av],CTime, 0, LogData),
    %NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    NewStateData = process_state(allred_move, StateData, [], [ca, av], null, street),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redred,Siblings},redgreen, NewStateData};
redred(idle,_From, StateData) ->
    io:format("All Red time. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings,Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("All Red time. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca, av],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(allred_idle, StateData, [], [ca, av], null, none),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redgreen,Siblings},redred, NewStateData};
    
redred(init_move_avenue,_From, StateData) ->
    io:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    LogData = find_element(log_data, StateData),
    write_result(LogData, io_lib:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    Siblings = find_element(siblings, StateData),
    {reply, {redred,Siblings},greenred, StateData};
redred(init_move_street,_From, StateData) ->
    io:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,    
    LogData = find_element(log_data, StateData),
    write_result(LogData, io_lib:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    Siblings = find_element(siblings, StateData),
    {reply, {redred,Siblings},redgreen, StateData}.


%% Moving AVENUES       
greenred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    
    %%{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %%write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_lanes(ManagedLanes, [av], [ca],CTime, GTime, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),    
    NewStateData = process_state(move,StateData, [av], [ca], null, avenue),
    {next_state, greenred, NewStateData};
greenred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    %%{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %%write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_lanes(ManagedLanes, [], [av, ca],CTime, 0, LogData),    
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %%NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [av, ca], greenred, avenue),
    {next_state, redred, NewStateData};
greenred(Event, StateData) ->
    unexpected_event(greenred, Event, StateData),
    {next_state, greenred, StateData}.

%%Synch calls
greenred(get_state, _From, StateData) ->
	ReturnData = get_state_data(greenred, StateData),
    {reply, ReturnData,greenred, StateData};
greenred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %%{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    ManagedLanes = find_element(managed_lanes, StateData),
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {greenred,DataLog},greenred, StateData};
greenred({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, greenred, OldState}, CtrlMod),
    write_checkpoint(greenred, DataLog, StateData),
    {reply, {greenred,DataLog}, greenred, StateData};
greenred({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,
    %io:format("Restore for: ~p~n",[LightId]),
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {greenred,RestoredData}, RestoredState, NewStateData};
greenred({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data(NewTimes, StateData),
    {reply, {greenred,NewTimes}, greenred, NewStateData};
greenred(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    %%{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %%write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_lanes(ManagedLanes, [av], [ca],CTime, GTime, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move, StateData, [av], [ca], null, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {greenred,Siblings},greenred, NewStateData};
greenred(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    %%{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %%write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_lanes(ManagedLanes, [], [av, ca],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %%NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [av, ca], greenred, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {greenred,Siblings},redred, NewStateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Moving STREETS    
redgreen(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    %%{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %%write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_lanes(ManagedLanes, [], [ca, av],CTime, GTime, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move, StateData, [ca], [av], null, street),
    {next_state, redgreen, NewStateData};
redgreen(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    %%{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %%write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_lanes(ManagedLanes, [], [ca, av],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %%NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [ca, av], redgreen, street),
    {next_state, redred, NewStateData};
redgreen(Event, StateData) ->
    unexpected_event(redgreen, Event, StateData),
    {next_state, redgreen, StateData}.      

%%Synch calls
redgreen(get_state, _From, StateData) ->
	ReturnData = get_state_data(redgreen, StateData),
    {reply, ReturnData,redgreen, StateData};
redgreen({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    ManagedLanes = find_element(managed_lanes, StateData),
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {redgreen,DataLog},redgreen, StateData};
redgreen({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, redgreen, OldState}, CtrlMod),
    write_checkpoint(redgreen, DataLog, StateData),
    {reply, {redgreen,DataLog}, redgreen, StateData};
redgreen({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,
    %io:format("Restore for: ~p~n",[LightId]),
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {redgreen,RestoredData}, RestoredState, NewStateData};
redgreen({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data(NewTimes, StateData),
    {reply, {redgreen,NewTimes}, redgreen, NewStateData};
redgreen(move_street,_From, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    %%{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %%{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %%write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_lanes(ManagedLanes, [av], [ca],CTime, GTime, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move, StateData, [ca], [av], null, street),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redgreen,Siblings},redgreen, NewStateData};
redgreen(idle,_From, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings,Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca,av],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [ca,av], redgreen, street),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redgreen,Siblings},redred, NewStateData}.
    
%% private functions
unexpected_event(_CurrentState, _Event, _StateData) ->
    io:format("traffic light error~n").
    
test() -> 
    {ok,final}.

%%INPUT: LIGHT info, current iteration
%%OUTPUT: None
%%Desc: CLIENT interface function to put each light to evaluate its state
evaluate_state({LightId, LightPid, Time}) ->
    io:format("Evaluating state ~w~n",[LightPid]),
   {State, Times, _Siblings, LogData, OldState, _CtrlMod} = get_state(LightPid),
   
   {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
   {go_time, GTime} = lists:keyfind(go_time, 1, Times),
   {allred_time, ARTime} = lists:keyfind(allred_time, 1, Times),
   {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
   
   write_result(LogData, 
       io_lib:format("Running simulation iteration: ~w continue",[Time])),
   write_result(LogData, io_lib:format("Evaluating state for light_fsm: ~w continue",[LightId])),

   NextTime = GTime + 1,   
   evaluate_state(State, OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid),
   write_endline(LogData).

%%INPUT: State =  current state, Nextime = number of the next iteration, ARTimer =  timer for all red lights
%%		 ARTime =  amount of time for all red lights, CTime = Cycle time (green light time)
%%OUTPUT: NONE
%%DESC: Evaluates the state according to times
evaluate_state(State, _OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid) when NextTime >= CTime, ARTimer <  ARTime->
   _NextDir = next_state_dir(State),
   io:format("Finishing ~w way cycle moving to idle~n",[State]),
   write_result(LogData, io_lib:format("Finishing ~w way cycle moving to idle",[State])),
   idle(LightPid);
   
evaluate_state(_State, greenred, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid) when NextTime >= CTime, ARTimer >=  ARTime->
   move_street(LightPid);
   
evaluate_state(_State, redgreen, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid) when NextTime >= CTime, ARTimer >=  ARTime->
   move_avenue(LightPid);   

evaluate_state(State = redred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   estimate_after_idle(LightPid, LogData);

evaluate_state(State = greenred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_avenue(LightPid);
   
evaluate_state(State = redgreen, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_street(LightPid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% WORKING

evaluate_state_new({LightId, LightPid, Time}) ->
    io:format("Evaluating state ~w~n",[LightPid]),
   {State, Times, _Siblings, LogData, OldState, CtrlMod} = get_state(LightPid),
   
   {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
   {go_time, GTime} = lists:keyfind(go_time, 1, Times),
   {allred_time, ARTime} = lists:keyfind(allred_time, 1, Times),
   {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
   
   write_result(LogData, 
       io_lib:format("Running simulation iteration: ~w continue",[Time])),
   write_result(LogData, io_lib:format("Evaluating state for light_fsm: ~w continue",[LightId])),

   NextTime = GTime + 1,   
   evaluate_state_new(State, OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid, CtrlMod),
   write_endline(LogData). 


evaluate_state_new(State, _OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid, CtrlMod) when NextTime >= CTime, ARTimer <  ARTime->
   io:format("Estimating time results for next state", []),
   Dir = next_state_dir(State),
   NewData = moduler:estimation_proc(CtrlMod, Dir),
   change_times({LightPid, NewData}),
   io:format("Finishing ~w way cycle moving to idle~n",[State]),
   write_result(LogData, io_lib:format("Finishing ~w way cycle moving to idle",[State])),   
   idle(LightPid);
   
evaluate_state_new(_State, greenred, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid, _CtrlMod) when NextTime >= CTime, ARTimer >=  ARTime->
   move_street(LightPid);
   
evaluate_state_new(_State, redgreen, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid, _CtrlMod) when NextTime >= CTime, ARTimer >=  ARTime->
   move_avenue(LightPid);   

evaluate_state_new(State = redred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid, _CtrlMod) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   estimate_after_idle(LightPid, LogData);

evaluate_state_new(State = greenred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid, _CtrlMod) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_avenue(LightPid);
   
evaluate_state_new(State = redgreen, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid, _CtrlMod) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_street(LightPid).





















   
%%%%%%
%%INPUT: ManagedLanes: all lanes connected to the light (av, ca, etc).
%%		 Active: key name (0 or 1) to find lanes to update on active
%%		 Idle:	 key names (1 or more) to find lanes to update on idle 
%%OUTPUT: None
%%DESC:	 This function is used to call all lanes and tell them to update either on idle or active

update_lanes(ManagedLanes, Active, Idle,CTime, GTime, LogData,Sensor) ->
	update_lanes_active_aux(ManagedLanes, Active, CTime, GTime, LogData, Sensor), 
    update_lanes_idle_aux(ManagedLanes, Idle, CTime, LogData).
	
update_lanes_active_aux(_ManagedLanes, [], _CTime, _GTime, _LogData, _Sensor)->
	[];
update_lanes_active_aux(ManagedLanes, [Active | Tail], CTime, GTime, LogData, Sensor)->
	Res = lists:keyfind(Active, 1, ManagedLanes),
	case Res of
    	{Active, List} -> update_on_active(List, CTime, GTime, LogData, Sensor);
    	false		   -> []		
    end,	
	update_lanes_active_aux(ManagedLanes, Tail, CTime, GTime, LogData, Sensor).

update_lanes_idle_aux(_ManagedLanes, [], _CTime, _LogData)->
	[];
update_lanes_idle_aux(ManagedLanes, [Idle | Tail], CTime, LogData)->
	Res = lists:keyfind(Idle, 1, ManagedLanes),
	case Res of
    	{Idle, List} -> update_on_idle(List, CTime, LogData);
    	false		   -> []		
    end,
	update_lanes_idle_aux(ManagedLanes, Tail, CTime, LogData).

%% Update each lane that is active on this light 
update_on_active([], _Cycle_time, _Go_time, _LogData, _Sensor) ->  
    true;
update_on_active([{LaneId,LanePid}|Tail], Go_time, Cycle_time, LogData, Sensor) ->  
    LanePid ! {go, self(), Cycle_time, Go_time, LogData, Sensor},
    receive
        {reply, _Reply} -> 
            %%io:format("reply recieve after update on active lane ~w.~n",[LaneId]),
            write_result(LogData, io_lib:format("reply recieve after update on active lane ~w",[LaneId]))
    end,
    update_on_active(Tail,Cycle_time, Go_time, LogData, Sensor).

%% Update each lane that is waiting on this light 
update_on_idle([], _Cycle_time, _LogData) -> 
    true;
update_on_idle([{LaneId,LanePid}|Tail], Cycle_time, LogData) ->
    LanePid ! {waiting, self(), Cycle_time, LogData},
    receive
        {reply, _Reply} -> 
            %%io:format("reply recieve after update on idle lane ~w.~n",[LaneId]),
            write_result(LogData, io_lib:format("reply recieve after update on idle lane ~w",[LaneId]))
    end,
    update_on_idle(Tail, Cycle_time, LogData).
    
estimate_after_idle(LightPid, LogData) ->
    case random:uniform(2) of
        1 -> io:format("First moving avenues ~w~n",[LightPid]),
             write_result(LogData, io_lib:format("First moving avenues ~w",[LightPid])),
             init_move_avenue(LightPid),
             move_avenue(LightPid);
        2 -> io:format("First moving streets ~w~n",[LightPid]),
             write_result(LogData, io_lib:format("First moving streets ~w",[LightPid])),
             init_move_street(LightPid),
             move_street(LightPid)
    end.
    
%% Write down results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).

write_final_data([], _Path) ->
	ok;
write_final_data([{_Dir, List} | ManagedLanes], Path) ->
    write_final_data_aux(List, Path),
    write_final_data(ManagedLanes, Path).
    
write_final_data_aux([], Path) ->
    write_result(Path, io_lib:format("FINISH WRITEDOWN FOR LANE",[])),
    write_result(Path, io_lib:format("=======================================",[]));
write_final_data_aux([{LaneId,LanePid}|Tail], Path) ->
    write_result(Path, io_lib:format("=======================================",[])),
    write_result(Path, io_lib:format("START WRITEDOWN FOR LANE: ~w",[LaneId])),
    
    LanePid ! {write_down, self(), Path,LaneId},
    receive
        {reply, _Reply} -> 
            io:format("reply recieve after writedown lane ~w.~n",[LaneId])            
    end,
    write_final_data_aux(Tail, Path).

write_endline(LogData) ->
   io:format("-----------------------------------------------~n~n"),
   write_result(LogData, io_lib:format("----------------------------------------------------",[])).
   
%%%%%%%%%%%%%%
%% CHECKPOINT
write_checkpoint(CallingState,DataLog, StateData) ->
	ManagedLanes = find_element(managed_lanes, StateData),
	LightId = find_element(id, StateData),	
	Times = find_element(times, StateData),
	OldState = find_element(old_state, StateData),
	CtrlMod = find_element(ctrl_mod, StateData),
	%io:format("Data to checkpoint  ~w~n", [{ManagedLanes, LightId, DataLog, {LightId, Times, CallingState, OldState}, CtrlMod}]),
	write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, CallingState, OldState}, CtrlMod).
	
write_checkpoint([], _LightId, DataLogs, LightStateData, CtrlMod) ->
	{LightChk, _LanesChk, _CarsChk, _OCarsChk} = DataLogs,
	filemanager:write_raw(LightChk, io_lib:format("~w", [LightStateData])),
	moduler:checkpoint(CtrlMod),
	{ok, checkpoint};
write_checkpoint([{Dir, List} | ManagedLanes], LightId, DataLogs, LightStateData, CtrlMod) ->	
	{ _LightChk, LanesChk, CarsChk,OCarsChk} = DataLogs,
    write_checkpoint_aux(List,{LightId, Dir, {LanesChk, CarsChk,OCarsChk}}),
    write_checkpoint(ManagedLanes, LightId, DataLogs, LightStateData, CtrlMod).
    
write_checkpoint_aux([], _LightData) ->
    [];
write_checkpoint_aux([{LaneId,LanePid}|Tail], LightData) ->   
    lane:checkpoint({LaneId, LanePid}, LightData),    
    write_checkpoint_aux(Tail, LightData).
    
    
%%%%%%%%%%%%%
%% State Data control
find_element(_Id, []) ->
	false;
%find_element(Id, {LightId, StateData}) ->
%	find_element(Id, StateData);
find_element(Id, StateData) ->
	Element = lists:keyfind(Id, 1, StateData),
	case Element of
		false ->	[];
		_Other ->	{Id, Value} = Element,
					Value
	end.


update_state_data([], NewStateData) ->
	NewStateData;
%update_state_data(Updates, {LightId, StateData}) ->
%	update_state_data(Updates, StateData);
update_state_data([{ElementId, NewValue} | UpdateTail], OldStateData) ->
	NewStateData = lists:keyreplace(ElementId,1, OldStateData, {ElementId, NewValue}),
	update_state_data(UpdateTail, NewStateData).

process_state(idle, StateData, OnActive, OnIdle, FromState, Dir) ->
	ManagedLanes = find_element(managed_lanes, StateData),
	Times = find_element(times, StateData),    
    CTime = find_element(cycle_time, Times),
    ARTimer = find_element(allred_timer, Times),
    LogData = find_element(log_data, StateData),

	write_result(LogData, io_lib:format("continue moving ~w lanes. Data: ~w",[Dir, StateData])),
    update_lanes(ManagedLanes, OnActive, OnIdle, CTime, 0, LogData, null),

    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    update_state_data([{times, NewTimes}, {old_state, FromState}], StateData);
    
process_state(move, StateData, OnActive, OnIdle, _FromState, Dir) ->
    ManagedLanes = find_element(managed_lanes, StateData),
    LogData = find_element(log_data, StateData),
    Times = find_element(times, StateData),
    CTime = find_element(cycle_time, Times),
    GTime = find_element(go_time, Times),
    
    %%NEW TO CONNECT WITH SENSOR
    CtrlMod = find_element(ctrl_mod, StateData),
    Sensor = get_safe_sensor(CtrlMod),
    
    write_result(LogData, io_lib:format("continue moving ~w lanes. Data: ~w",[Dir, StateData])),
    update_lanes(ManagedLanes, OnActive, OnIdle,CTime, GTime, LogData, Sensor),    
    
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    update_state_data([{times, NewTimes}], StateData);

process_state(allred_move, StateData, OnActive, OnIdle, _FromState, Dir) ->
	ManagedLanes = find_element(managed_lanes, StateData),
    LogData = find_element(log_data, StateData),
    Times = find_element(times, StateData),
    CTime = find_element(cycle_time, Times),
        
    write_result(LogData, io_lib:format("Changing for red to green on ~w. Start Moving ~w lanes. Data: ~w~n",[Dir, Dir,StateData])),    
    update_lanes(ManagedLanes, OnActive, OnIdle,CTime, 0, LogData, null),
    
    NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    
    update_state_data([{times, NewTimes}], StateData);
	
process_state(allred_idle, StateData, OnActive, OnIdle, _FromState, _Dir) ->
    ManagedLanes = find_element(managed_lanes, StateData),
    LogData = find_element(log_data, StateData),
    Times = find_element(times, StateData),
    CTime = find_element(cycle_time, Times),
    ARTimer = find_element(allred_timer, Times),
    
    write_result(LogData, io_lib:format("All Red time. Data: ~w~n",[StateData])),
    update_lanes(ManagedLanes, OnActive, OnIdle, CTime, 0, LogData, null),    
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    
    update_state_data([{times, NewTimes}], StateData).

get_state_data(State, StateData) ->
	Times = find_element(times, StateData),
	Siblings = find_element(siblings, StateData),
    LogData = find_element(log_data, StateData),
    OldState = find_element(old_state, StateData),
    CtrlMod = find_element(ctrl_mod, StateData),
	{State,Times,Siblings, LogData, OldState, CtrlMod}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MODULER interface

%%DESC: Basic function on init just to update information of the moduler related with connections
update_moduler(StateData, Siblings) ->
	CtrlModSender = find_element(ctrl_mod, StateData),
	SenderId = find_element(id, StateData),
	lists:map(
		fun ({Dir, SiblingsList}) ->
			io:format("Sibling list for ~w : ~w~n",[Dir, SiblingsList]),
			lists:map(
				%fun({ReceiverId, _LightPid,_Sequence, CtrlModReciever}) ->
				fun({ReceiverId, _LightPid,_Sequence, CtrlModReciever, RecieverLocation}) ->					
				  	%moduler:connect(Dir, {SenderId, CtrlModSender}, {ReceiverId, CtrlModReciever}),
				  	case RecieverLocation of
				  		antes -> SenderLocation = despues;
				  		despues -> SenderLocation = antes
				  	end,
				  	%moduler:connect(Dir, {SenderId, CtrlModSender}, {ReceiverId, CtrlModReciever}),
				  	moduler:connect(Dir, {SenderId, CtrlModSender,SenderLocation}, {ReceiverId, CtrlModReciever, RecieverLocation}),
				  	io:format("moduler connected~w~n",[ReceiverId]),
				  	moduler:status(CtrlModSender)
				  	%moduler:status(CtrlModReciever)
				end,
				SiblingsList	
		)
		end,
		Siblings
	), 
	{ok, upd_moduler}.
	
%%INPUT: StateData all data related to the light
%%OUTPUT: None
%%DESC: Scans all lanes in order to get relevant information about them like obstructions, max speed, capacity, etc
%% and after that sends values to the moduler
scan_lanes(StateData) ->
	ManagedLanes = find_element(managed_lanes, StateData),
	CtrlMod = find_element(ctrl_mod, StateData),
	Info = scan_lanes_aux(ManagedLanes, []),
	CtrlMod ! {inputs,self(), Info},
	{ok, scanned}.
	
scan_lanes_aux([], Scanned) ->
	Scanned;
scan_lanes_aux([{Dir, List} | ManagedLanes], Scanned) ->
	InfoList = lists:map(
		fun({LaneId,LanePid}) ->
			LanePid ! {info, self()},
			receive
				{reply, false} -> {LaneId,LanePid};
				{reply, Data} ->  Data
			end
		end,
		List
	),
	ProcessedList = process_scans(InfoList),
	scan_lanes_aux(ManagedLanes, [{Dir, ProcessedList}|Scanned]).
	
process_scans(InfoList) ->
	CountType1 = length(lists:filter(fun({_LaneId, Type,_Capacity,_Obstruction,_TopSpeed}) -> Type == 1 end, InfoList)),
	CountType2 = length(lists:filter(fun({_LaneId, Type,_Capacity,_Obstruction,_TopSpeed}) -> Type == 2 end, InfoList)),
	BaseLine = lists:last(InfoList),
	{_LaneId, _Type, BCapacity, _Obstruction, BTopSpeed} = BaseLine,
	CountObStops = length(lists:filter(fun({_Id,_Dirs,_Capacity,Obstruction,_TopSpeed}) -> 
											lists:any(fun({Obs, _Begin, _End}) ->  Obs =:= park end,Obstruction) == true
									   end,
									   InfoList)),
	%{CountType1, CountType2, BCapacity, BTopSpeed, CountObStops}.
	[{count_type1, CountType1},{count_type2, CountType2}, {capacity, BCapacity}, {top_speed, BTopSpeed}, {count_obs, CountObStops}].


get_safe_sensor(CtrlMod) ->
	Res = moduler:get_sensor(CtrlMod),
	case Res of
		{reply, Sensor} -> Sensor;
		_Other			-> null
	end.


%%INPUT: CurrentState... state of the traffic light
%%OUTPUT: Next state dir(av, ca, avf, etc..) the key to find the data for the next lanes to move
%%DESC: according to the current state gets the direction of the next state.
next_state_dir(greenred) ->
	ca;
next_state_dir(redgreen) ->
	av;	
next_state_dir(_CurrentState) ->	
	null.

%%INPUT: moduler response to state
%%OUTPUT: none
%%DESC:  Update for the light with the information received from the moduler
%%		this changes, times and other variables handled by it		
%update_data(NewData) ->
%	NewStateData = update_state_data([{times, NewTimes}], StateData).


%%DESC: Format the data returned by the moduler
%format_moduler_data(NewData) ->
%	true.