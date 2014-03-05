-module(light_fsm_3st).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         get_state/1]).

%states
-export([ allred/2, allred/3, greenredred/2, greenredred/3, redgreenred/2, redgreenred/3, redredgreen/2, redredgreen/3]).
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
    
move_avenueF(Pid) ->
    gen_fsm:sync_send_event(Pid,move_avenueF).

idle(Pid) ->
    gen_fsm:sync_send_event(Pid,idle).

force(Pid) ->
    gen_fsm:sync_send_event(Pid,force).

update_siblings({Pid, Siblings}) ->
    gen_fsm:sync_send_event(Pid,{update_siblings, Siblings}).

tabulate_data({Pid, DataLog}) ->
    gen_fsm:sync_send_event(Pid,{tabulate_data, DataLog}).
    
checkpoint_data({Pid, DataLog}) ->
    gen_fsm:sync_send_event(Pid,{checkpoint, DataLog}).

restore({Pid, RestoredData}) ->
    gen_fsm:sync_send_event(Pid,{restore, RestoredData}).

change_times({Pid, NewTimes}) ->
    gen_fsm:sync_send_event(Pid,{change_times, NewTimes});
change_times({Pid, NewTimes, DirList}) ->
    gen_fsm:sync_send_event(Pid,{change_times, NewTimes, DirList}).
 
%% gen_fsm functions

start_link(Args) ->
    gen_fsm:start_link(?MODULE,Args,[]).

init(Args) ->
    %%{LightId, {Av, Ca, AvF},Siblings, Cycle_time, Go_time, LogData} = Args,
    {Mode, LightId, ManagedLanes,Siblings, Times, LogData, LightMode} = Args,
    file:delete(LogData), %% delete old log
    NewTimes = [{allred_timer, 0} | Times],
    
    CtrlMod = moduler:start({Mode, LightId, ManagedLanes}),
    %{ok, allred,{LightId, ManagedLanes,Siblings, NewTimes, LogData, allred, CtrlMod}}.
    StateData = [{id,LightId}, {managed_lanes, ManagedLanes},{siblings, Siblings}, 
    	{times, NewTimes}, {log_data, LogData}, {old_state, allred}, {ctrl_mod, CtrlMod}, {light_mode, LightMode}],
    
    case Mode of
    	normal -> scan_lanes(StateData);
    	_Other -> continue
    end,
    
    {ok, allred, StateData}.

get_state({LightPid, TargetsList}) ->
    try
        gen_fsm:sync_send_event(LightPid, {get_state, TargetsList})
    catch 
	exit : { noproc, _ } -> closed
    end;


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
allred(move_avenue, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId, ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewStateData = process_state(allred_move, StateData, [], [av, ca, avf], null, avenue),
    {next_state, greenredred, NewStateData};
allred(move_street, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),    
    %{LightId, ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewStateData = process_state(allred_move, StateData, [], [ca, avf, av], null, street),
    {next_state, redgreenred, NewStateData};
allred(move_avenueF, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId, ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewStateData = process_state(allred_move, StateData, [], [avf, av, ca], null, avenueF),
    {next_state, redredgreen, NewStateData};
allred(Event, StateData) ->
    unexpected_event(allred, Event, StateData),
    {next_state, allred, StateData}.

%%Synch calls
allred(get_state, _From, StateData) ->
	ReturnData = get_state_data(allred, StateData),
    {reply, ReturnData, allred, StateData};

allred({get_state, TargetList}, _From, StateData) ->
	ReturnData = get_state_data(allred, StateData, TargetList),
    {reply, ReturnData,allred, StateData};

allred({update_siblings, Siblings},_From, StateData) ->
    %io:format("Updating StateData: ~w~n",[Siblings]),
    %{LightId,ManagedLanes,_Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    update_moduler(StateData, Siblings),
    NewStateData = update_state_data([{siblings, Siblings}], StateData),    
    {reply, {allred,Siblings},allred, NewStateData};
allred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    ManagedLanes = find_element(managed_lanes, StateData),
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {allred,DataLog},allred, StateData};
allred({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, allred, OldState}, CtrlMod),
    
    write_checkpoint(allred, DataLog, StateData),    
    {reply, {allred,DataLog}, allred, StateData};
allred({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,
    %io:format("Restore for: ~p~n",[LightId]),
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {allred,RestoredData}, RestoredState, NewStateData};
allred({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData),
    {reply, {allred,NewTimes}, allred, NewStateData};
allred({change_times, NewTimes, DirList}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData, DirList),
    {reply, {allred,NewTimes}, allred, NewStateData};
allred(move_avenue,_From, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    NewStateData = process_state(allred_move, StateData, [], [av, ca, avf], null, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},greenredred, NewStateData};
allred(move_street,_From, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    NewStateData = process_state(allred_move, StateData, [], [ca, avf, av], null, street),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},redgreenred, NewStateData};
allred(move_avenueF,_From, StateData) ->
    io:format("Changing for red to green on avenueF. Start Moving avenueF lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    NewStateData = process_state(allred_move, StateData, [], [avf, av, ca], null, avenueF),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},redredgreen, NewStateData};
allred(idle,_From, StateData) ->
    io:format("All Red time. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings,Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("All Red time. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(allred_idle, StateData, [], [av, ca, avf], null, none),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},allred, NewStateData};
allred(force, _From, StateData) ->
	io:format("Force change on light ~n"),
	NewStateData = force_time(StateData),
	{reply, {allred, null},allred, NewStateData};
    
allred(init_move_avenue,_From, StateData) ->
    io:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    LogData = find_element(log_data, StateData),
    NewStateData = update_cycle_time_on_statedata(avenue, StateData),
    write_result(LogData, io_lib:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[NewStateData])),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},greenredred, NewStateData};
allred(init_move_street,_From, StateData) ->
    io:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    LogData = find_element(log_data, StateData),
    NewStateData = update_cycle_time_on_statedata(street, StateData), 
    write_result(LogData, io_lib:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[NewStateData])),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},redgreenred, NewStateData};
allred(init_move_avenueF,_From, StateData) ->
    io:format("First move of lights from red to green on avenueF. Start Moving avenueF lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    LogData = find_element(log_data, StateData),
    NewStateData = update_cycle_time_on_statedata(avenueF, StateData),
    write_result(LogData, io_lib:format("First move of lights from red to green on avenueF. Start Moving avenueF lanes. Data: ~w~n",[NewStateData])),
    Siblings = find_element(siblings, NewStateData),
    {reply, {allred,Siblings},redredgreen, NewStateData}.


%% Moving AVENUES       
greenredred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %update_lanes(ManagedLanes, [av], [ca,avf],CTime, GTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move,StateData, [av], [ca,avf], null, avenue),
    {next_state, greenredred, NewStateData};
greenredred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [av,ca,avf], greenredred, avenue),
    {next_state, allred, NewStateData};
greenredred(Event, StateData) ->
    unexpected_event(greenredred, Event, StateData),
    {next_state, greenredred, StateData}.

%%Synch calls
greenredred(get_state, _From, StateData) ->
	ReturnData = get_state_data(greenredred, StateData),
    {reply, ReturnData,greenredred, StateData};

greenredred({get_state, TargetList}, _From, StateData) ->
	ReturnData = get_state_data(greenredred, StateData, TargetList),
    {reply, ReturnData,greenredred, StateData};    
 
greenredred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
	ManagedLanes = find_element(managed_lanes, StateData),    	
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {greenredred,DataLog},greenredred, StateData};
greenredred({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, greenredred, OldState}, CtrlMod),
    write_checkpoint(greenredred, DataLog, StateData),
    {reply, {greenredred,DataLog}, greenredred, StateData};
greenredred({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,
    %io:format("Restore for: ~p~n",[LightId]),
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {greenredred,RestoredData}, RestoredState, NewStateData};
greenredred({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData),
    {reply, {greenredred,NewTimes}, greenredred, NewStateData};
greenredred({change_times, NewTimes,DirList}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData, DirList),
    {reply, {greenredred,NewTimes}, greenredred, NewStateData};
greenredred(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [av], [ca,avf],CTime, GTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move,StateData, [av], [ca,avf], null, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {greenredred,Siblings},greenredred, NewStateData};
greenredred(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [av,ca,avf], greenredred, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {greenredred,Siblings},allred, NewStateData};
greenredred(force, _From, StateData) ->
	io:format("Force change on light ~n"),
	NewStateData = force_time(StateData),
	{reply, {greenredred, null},greenredred, NewStateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Moving STREETS    
redgreenred(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [ca], [av,avf],CTime, GTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move, StateData, [ca], [av,avf], null, street),
    {next_state, redgreenred, NewStateData};
redgreenred(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [ca,avf,av], redgreenred, street),
    {next_state, allred, NewStateData};
redgreenred(Event, StateData) ->
    unexpected_event(redgreenred, Event, StateData),
    {next_state, redgreenred, StateData}. 

%%Synch calls     
redgreenred(get_state, _From, StateData) ->
	ReturnData = get_state_data(redgreenred, StateData),
    {reply, ReturnData,redgreenred, StateData};
    
redgreenred({get_state, TargetList}, _From, StateData) ->
	ReturnData = get_state_data(redgreenred, StateData, TargetList),
    {reply, ReturnData,redgreenred, StateData};    
    
redgreenred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    ManagedLanes = find_element(managed_lanes, StateData),
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {redgreenred,DataLog},redgreenred, StateData};
redgreenred({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, redgreenred, OldState}, CtrlMod),
    write_checkpoint(redgreenred, DataLog, StateData),
    {reply, {redgreenred,DataLog}, redgreenred, StateData};
redgreenred({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,
    %io:format("Restore for: ~p~n",[LightId]),
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {redgreenred,RestoredData}, RestoredState, NewStateData};
redgreenred({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData),
    {reply, {redgreenred,NewTimes}, redgreenred, NewStateData};
redgreenred({change_times, NewTimes, DirList}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData, DirList),
    {reply, {redgreenred,NewTimes}, redgreenred, NewStateData};
redgreenred(move_street,_From, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [ca], [av,avf],CTime, GTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move, StateData, [ca], [av, avf], null, street),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redgreenred,Siblings},redgreenred, NewStateData};
redgreenred(idle,_From, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings,Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [ca,av,avf], redgreenred, street),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redgreenred,Siblings},allred, NewStateData}.
    
%% Moving OTHER AVENUES (LIGHT WITH to avenues different direction)
redredgreen(move_avenueF, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(AvF, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %update_lanes(ManagedLanes, [avf], [ca,av],CTime, GTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move,StateData, [avf], [ca,av], null, avenue),
    {next_state, greenredred, NewStateData};
redredgreen(idle, StateData) ->
    %io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [avf, av, ca], redredgreen, avenue),
    {next_state, allred, NewStateData};
redredgreen(Event, StateData) ->
    unexpected_event(greenredred, Event, StateData),
    {next_state, greenredred, StateData}.

%%Synch calls
redredgreen(get_state, _From, StateData) ->
	ReturnData = get_state_data(redredgreen, StateData),
    {reply, ReturnData,redredgreen, StateData};
    
redredgreen({get_state, TargetList}, _From, StateData) ->
	ReturnData = get_state_data(redredgreen, StateData, TargetList),
    {reply, ReturnData,redredgreen, StateData}; 
    
redredgreen({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    ManagedLanes = find_element(managed_lanes, StateData),
    write_final_data(ManagedLanes, DataLog),
    CtrlMod = find_element(ctrl_mod, StateData),
    moduler:stop(CtrlMod),
    {reply, {redredgreen,DataLog},redredgreen, StateData};
redredgreen({checkpoint, DataLog}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %io:format("Checkpoint for: ~p~n",[LightId]),
    %write_checkpoint(ManagedLanes, LightId, DataLog, {LightId, Times, redredgreen, OldState}, CtrlMod),
    write_checkpoint(redredgreen, DataLog, StateData),
    {reply, {redredgreen,DataLog}, redredgreen, StateData};
redredgreen({restore, RestoredData}, _From, StateData) ->    
    %{LightId,ManagedLanes, Siblings, _Times, LogData, _OldState, CtrlMod} = StateData,
    {_Id, RestoredTimes, RestoredState, RestoredOldState} = RestoredData,
    %io:format("Restore for: ~p~n",[LightId]),
    NewStateData = update_state_data([{times, RestoredTimes}, {old_state, RestoredOldState}], StateData),
    {reply, {redredgreen,RestoredData}, RestoredState, NewStateData};
redredgreen({change_times, NewTimes}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData),
    {reply, {redredgreen,NewTimes}, redredgreen, NewStateData};
redredgreen({change_times, NewTimes, DirList}, _From, StateData) ->
	io:format("New Times update~n",[]),
    NewStateData = update_state_data_times(NewTimes, StateData, DirList),
    {reply, {redredgreen,NewTimes}, redredgreen, NewStateData};
redredgreen(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{go_time, GTime} = lists:keyfind(go_time, 1, Times),
    %write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(AvF, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %update_lanes(ManagedLanes, [avf], [ca,av],CTime, GTime, LogData),
    %NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    NewStateData = process_state(move, StateData, [avf], [ca,av], null, avenuef),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redredgreen,Siblings},redredgreen, NewStateData};
redredgreen(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    %{LightId,ManagedLanes,Siblings, Times, LogData, _OldState, CtrlMod} = StateData,
    %{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    %{allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    %write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    %update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    %NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    NewStateData = process_state(idle, StateData, [], [avf,av,ca], redredgreen, avenue),
    Siblings = find_element(siblings, NewStateData),
    {reply, {redredgreen,Siblings},allred, NewStateData};
redredgreen(force, _From, StateData) ->
	io:format("Force change on light ~n"),
	NewStateData = force_time(StateData),
	{reply, {redredgreen, null},redredgreen, NewStateData}.   
    
    
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
   {State, Times, _Siblings, LogData, OldState, CtrlMod, LightMode, ManagedLanes} = get_state(LightPid),
   
   {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
   {go_time, GTime} = lists:keyfind(go_time, 1, Times),
   {allred_time, ARTime} = lists:keyfind(allred_time, 1, Times),
   {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
   Delay = find_element(delay, Times),
   MaxWait = find_element(umbral, Times),
					
   write_result(LogData, 
       io_lib:format("Running simulation iteration: ~w continue",[Time])),
   write_result(LogData, io_lib:format("Evaluating state for light_fsm: ~w continue",[LightId])),

   NextTime = GTime + 1,
   io:format("LightMode: ~w~n",[LightMode]),
   case LightMode of
	default ->  evaluate_state(State, OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid);
		dm		->  evaluate_state_dm(State, OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid, CtrlMod, Delay, MaxWait, ManagedLanes);
		dual	->  evaluate_state_dual(State, OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid, CtrlMod, Delay, MaxWait, ManagedLanes)
   end,
   
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
   
evaluate_state(_State, greenredred, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid) when NextTime >= CTime, ARTimer >=  ARTime->
   move_street(LightPid);
   
evaluate_state(_State, redgreenred, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid) when NextTime >= CTime, ARTimer >=  ARTime->
   move_avenueF(LightPid);  
   
evaluate_state(_State, redredgreen, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid) when NextTime >= CTime, ARTimer >=  ARTime->
   move_avenue(LightPid);   

evaluate_state(State = allred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   estimate_after_idle(LightPid, LogData);

evaluate_state(State = greenredred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_avenue(LightPid);
   
evaluate_state(State = redgreenred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_street(LightPid);
   
evaluate_state(State = redredgreen, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid) when NextTime < CTime ->
   io:format("Continuing ~w way cycle~n",[State]),
   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
   move_avenueF(LightPid).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% WORKING

evaluate_state_dm(State, _OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid, 
  CtrlMod, _Delay, _MaxWait, ManagedLanes) when NextTime >= CTime, ARTimer <  ARTime->
  	io:format("Estimating time results for next state~n", []),
  	CarStats = eval_delay(ManagedLanes),  %%get cars on lane (all lanes)
  	%CarStats = [],
	DirList = next_state_dir(State),
   	{reply, NewData} = moduler:estimation_proc(CtrlMod, DirList,CarStats),
   	io:format("NewData after moduler call ~w~n",[NewData]),
   	change_times({LightPid, NewData, DirList}),
   	io:format("Finishing ~w way cycle moving to idle~n",[State]),
   	write_result(LogData, io_lib:format("Finishing ~w way cycle moving to idle",[State])),   
	idle(LightPid);
   
evaluate_state_dm(_State, greenred, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid, 
  CtrlMod, _Delay, _MaxWait, _ManagedLanes) when NextTime >= CTime, ARTimer >=  ARTime->
    Dir = current_state_dir(greenred),
    moduler:reset_sensor(CtrlMod, Dir),
    move_street(LightPid);
   
evaluate_state_dm(_State, redgreen, NextTime, CTime, ARTime, ARTimer, _LogData, LightPid, 
  CtrlMod, _Delay, _MaxWait, _ManagedLanes) when NextTime >= CTime, ARTimer >=  ARTime->
    Dir = current_state_dir(greenred),
    moduler:reset_sensor(CtrlMod, Dir),
    move_avenue(LightPid);   

evaluate_state_dm(State = redred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid, 
  _CtrlMod, _Delay, _MaxWait, _ManagedLanes) when NextTime < CTime ->
    io:format("Continuing ~w way cycle~n",[State]),
    write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
    estimate_after_idle(LightPid, LogData);

evaluate_state_dm(State = greenred, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid, 
  CtrlMod, _Delay, MaxWait, _ManagedLanes) when NextTime < CTime ->
    io:format("Continuing ~w way cycle~n",[State]),
    write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
    move_avenue(LightPid),
    evaluate_umbral(CtrlMod, MaxWait, LightPid);
   
evaluate_state_dm(State = redgreen, _OldState, NextTime, CTime, _ARTime, _ARTimer, LogData, LightPid, 
  CtrlMod, _Delay, MaxWait, _ManagedLanes) when NextTime < CTime ->
    io:format("Continuing ~w way cycle~n",[State]),
    write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
    move_street(LightPid),
	evaluate_umbral(CtrlMod, MaxWait, LightPid).




evaluate_umbral(CtrlMod, MaxWait, LightPid) ->
	Res = moduler:check_sensor_standby(CtrlMod, MaxWait),
    case Res of
    	{reply, change} ->
    		force(LightPid);
    	_Other  ->
    		ok
    end.







evaluate_state_dual(_State, _OldState, _NextTime, _CTime, _ARTime, _ARTimer, _LogData, _LightPid, 
  _CtrlMod, _Delay, _MaxWait, _ManagedLanes) -> true.



















   

%%%%%%
%%INPUT: ManagedLanes: all lanes connected to the light (av, ca, etc).
%%		 Active: key name (0 or 1) to find lanes to update on active
%%		 Idle:	 key names (1 or more) to find lanes to update on idle 
%%OUTPUT: None
%%DESC:	 This function is used to call all lanes and tell them to update either on idle or active

update_lanes(ManagedLanes, Active, Idle,CTime, GTime, LogData, Sensor) ->
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
write_checkpoint(CallingState, DataLog, StateData) ->
	ManagedLanes = find_element(managed_lanes, StateData),
	LightId = find_element(id, StateData),
	Times = find_element(times, StateData),
	OldState = find_element(old_state, StateData),
	CtrlMod = find_element(ctrl_mod, StateData),
	io:format("Data to checkpoint  ~w~n", [{ManagedLanes, LightId, DataLog, {LightId, Times, CallingState, OldState}, CtrlMod}]),
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
    
    %Change cycle_time for the next state
    %TempTimes = get_state_cycle_time(Dir, Times),
    update_next_cycle_on_allred(Dir, Times, StateData);
    
    %update_state_data([{times, NewTimes}], StateData);
	
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
    LightMode = find_element(light_mode, StateData),
	{State,Times,Siblings, LogData, OldState, CtrlMod, LightMode}.
	
get_state_data(State, StateData, TargetList) ->
	Res = lists:map(fun(Key) ->	find_element(Key, StateData) end, TargetList),
	list_to_tuple([State | Res]).	
	
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
				  	io:format("moduler connected~w~n",[ReceiverId])
				  	%moduler:status(CtrlModSender)
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
next_state_dir(greenredred) ->
	[ca, avf, av];
next_state_dir(redgreenred) ->
	[avf, av, ca];
next_state_dir(redredgreen) ->
	[av, ca, avf];	
next_state_dir(_CurrentState) ->	
	null.
	
%%INPUT: CurrentState... state of the traffic light
%%OUTPUT: current state dir(av, ca, avf, etc..) the key to find the data for the next lanes to move
%%DESC: according to the current state gets the direction of the next state.
current_state_dir(greenred) ->
	av;
current_state_dir(redgreen) ->
	ca;	
current_state_dir(_CurrentState) ->	
	null.

%%INPUT: CurrentState... state of the traffic light
%%OUTPUT: current state dir(av, ca, avf, etc..) the key to find the data for the next lanes to move
%%DESC: according to the current state gets the direction of the next state.
current_dir_state_key(av) ->
	avenue_time;
current_dir_state_key(ca) ->
	street_time;
current_dir_state_key(avf) ->
	avenueF_time;
current_dir_state_key(_CurrentState) ->	
	null.	
	
%%INPUT: StateData current state data
%%OUTPUT: updated stateData
force_time(StateData)->
	Times = find_element(times, StateData),
	Cycle_Time = find_element(cycle_time, Times),
	NewTimes = lists:keyreplace(go_time, 1, Times, {go_time,Cycle_Time}),	
	update_state_data([{times, NewTimes}], StateData).

%%INPUT: CurrentStateData... data of the light for the current state
%%		 NewVals : new times to update in stateData
%%OUTPUT: Updated statedata
%%DESC: gets target times for param newvals and update times with them.
update_state_data_times(NewVals, StateData) ->
	Times = find_element(times, StateData),
	FilterVals = [{X, Y} || {X, Y} <- NewVals, (X == cycle_time) or (X == delay) or (X == umbral)],
	NewTimes = update_state_data(FilterVals, Times),
	update_state_data([{times, NewTimes}], StateData).


%%INPUT: CurrentStateData... data of the light for the current state
%%		 NewVals : new times to update in stateData
%%		 DirList : lists of dirs to update
%%OUTPUT: Updated statedata
%%DESC: gets target times for param newvals and update times with them.
update_state_data_times(NewVals, StateData, DirList) ->
	io:format("OLD STATE DATA B4 TIMES UPDATE: ~w~n~n",[StateData]),
	NewStateData = lists:foldl(fun(Dir, Acc) ->
		io:format("CHANGE STATE DATA TIMES ~w~n~n",[NewVals]),
		
		%%Get the values to update for the current Dir in loop
		UpdateVals = find_element(Dir, NewVals),
		Times = find_element(times, Acc),
		PreFilterTempVals = [{X, Y} || {X, Y} <- UpdateVals, (X == cycle_time) or (X == delay) or (X == umbral)],		
		NewCycleTime = find_element(cycle_time, PreFilterTempVals),
		
		%%Get the key to fin current Dir data
		DirKey = current_dir_state_key(Dir),
		DirTimeKey = list_to_atom(string:concat(atom_to_list(DirKey), "_time")),
		PreFilterVals = fix_times_keys(PreFilterTempVals, DirKey),
		io:format("FILTERED VALS AFTER KEY FIX ~w~n~n",[PreFilterVals]),
		FilterVals = [{DirTimeKey, NewCycleTime} | PreFilterVals],
		io:format("TIMES TO UPDATE~w~n~n",[FilterVals]),
		
		%%Update times
		NewTimes = update_state_data(FilterVals, Times),
		io:format("UPDATED TIMES~w~n~n",[NewTimes]),
		update_state_data([{times, NewTimes}], Acc)
		end,
		StateData,
		DirList),
	io:format("New STATE DATA AFTER TIMES UPDATE: ~w~n~n",[NewStateData]),
	NewStateData.
		
fix_times_keys(List, Dir) ->
	SDir = atom_to_list(Dir),
	lists:map(
		fun({Key, Value}) ->
			NewKey = 
				if Key /= cycle_time ->
					S = "_" ++ atom_to_list(Key),
					list_to_atom(string:concat(SDir, S));
				   true ->
				   	Key
				end,
			{NewKey, Value}
		end,
		List
	).

%%INPUT: CurrentStateData... data of the light for the current state
%%		 
%%OUTPUT: list all cars that are in the current lane
%%DESC:	iterate through all lanes and get a unique list of cars with all information
%% 		according to the desired delay for the lane.
eval_delay(ManagedLanes) ->
	io:format("~nEVAL DELAY ~w ~n~n",[ManagedLanes]),
	List = 
		lists:foldl(fun({_Dir, Lanes}, Acc) ->
			[eval_delay_aux(Lanes) | Acc]
		 end,
		 [],
		 ManagedLanes),
	
	io:format("~nEVAL DELAY RESULT ~w ~n~n",[List]),
	List.
	 %lists:append(List).


eval_delay_aux([]) -> [];
eval_delay_aux(Lanes = [{LaneId,_LanePid} | _Tail]) ->
	S = atom_to_list(LaneId),
	ParentLaneId = list_to_atom(string:concat(string:substr(S,8,2), string:substr(S,5,3))),
	CarStats = lists:append(lists:map(
		fun({_LaneId,LanePid})->
			LanePid ! {info_cars, self()},
			receive
				{reply, Cars} -> Cars			 
			end
		end,
		Lanes
	)),
	{ParentLaneId, CarStats}.

%%INPUT: Dir = (avenue, street, etc...), 
%%		 Times = lists of all times for the light
%%OUTPUT cycle time for the next state
%%Get cycle time for next state	 
%get_state_cycle_time(avenue, Times)->
%	Time = find_element(avenue_time, Times),
%	Time;
%get_state_cycle_time(avenueF, Times)->
%	Time = find_element(avenueF_time, Times),
%	Time;
%get_state_cycle_time(street, Times)->
%	Time = find_element(street_time, Times),
%	Time.
	
%%Update cycle time for state on state
update_cycle_time_on_statedata(avenue, StateData) ->
	update_cycle_time_on_statedata_aux(avenue_time, StateData);
update_cycle_time_on_statedata(avenueF, StateData) ->
	update_cycle_time_on_statedata_aux(avenueF_time, StateData);
update_cycle_time_on_statedata(street, StateData) ->
	update_cycle_time_on_statedata_aux(street_time, StateData).

update_cycle_time_on_statedata_aux(Key, StateData) ->
	Times = find_element(times, StateData),
	CycleTime = find_element(Key, Times),
	NewTimes = lists:keyreplace(cycle_time, 1, Times, {cycle_time, CycleTime}),
	update_state_data([{times, NewTimes}], StateData).
	
update_next_cycle_on_allred(Dir, Times, StateData) ->
	NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    TempStateData = update_state_data([{times, NewTimes}], StateData),    
    update_cycle_time_on_statedata(Dir, TempStateData).
