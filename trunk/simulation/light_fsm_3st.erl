-module(light_fsm_3st).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         get_state/1, tabulate_data/2]).

%states
-export([ allred/2, allred/3, greenredred/2, greenredred/3, redgreenred/2, redgreenred/3, redredgreen/2, redredgreen/3]).
%-record(state,{}).

%client calls
-export([start_link/1,move_avenue/1, move_street/1, idle/1,update_siblings/2, evaluate_state/1]).

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

update_siblings(Pid, Siblings) ->
    gen_fsm:sync_send_event(Pid,{update_siblings, Siblings}).

tabulate_data(Pid, DataLog) ->
    gen_fsm:sync_send_event(Pid,{tabulate_data, DataLog}).
%% gen_fsm functions

start_link(Args) ->
    gen_fsm:start_link(?MODULE,Args,[]).

init(Args) ->
    %%{LightId, {Av, Ca, AvF},Siblings, Cycle_time, Go_time, LogData} = Args,
    {LightId, ManagedLanes,Siblings, Times, LogData} = Args,
    file:delete(LogData), %% delete old log
    NewTimes = [{allred_timer, 0} | Times],
    {ok, allred,{LightId, ManagedLanes,Siblings, NewTimes, LogData, allred}}.

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
    {LightId, ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    {next_state, greenredred, {LightId, ManagedLanes,Siblings, NewTimes, LogData, OldState}};
allred(move_street, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),    
    {LightId, ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    {next_state, redgreenred, {LightId, ManagedLanes, Siblings, NewTimes, LogData, OldState}};
allred(move_avenueF, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId, ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    {next_state, redredgreen, {LightId, ManagedLanes,Siblings, NewTimes, LogData, OldState}};
allred(Event, StateData) ->
    unexpected_event(allred, Event, StateData),
    {next_state, allred, StateData}.

%%Synch calls
allred(get_state, _From, StateData = {_LightId, _ManagedLanes,Siblings, Times, LogData, OldState}) ->
    {reply, {allred,Times,Siblings, LogData, OldState},allred, StateData};
allred({update_siblings, Siblings},_From, StateData) ->
    %io:format("Updating StateData: ~w~n",[Siblings]),
    {LightId,ManagedLanes,_Siblings, Times, LogData, OldState} = StateData,
    {reply, {allred,Siblings},allred, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}};
allred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {allred,DataLog},allred, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}};
allred(move_avenue,_From, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    {reply, {allred,Siblings},greenredred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
allred(move_street,_From, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    {reply, {allred,Siblings},redgreenred, {LightId,ManagedLanes, Siblings, NewTimes, LogData, OldState}};
allred(move_avenueF,_From, StateData) ->
    io:format("Changing for red to green on avenueF. Start Moving avenueF lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    {reply, {allred,Siblings},redredgreen, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
allred(idle,_From, StateData) ->
    io:format("All Red time. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings,Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("All Red time. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {redgreenred,Siblings},allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
    
allred(init_move_avenue,_From, StateData) ->
    io:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    write_result(LogData, io_lib:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    {reply, {allred,Siblings},greenredred, {LightId,ManagedLanes,Siblings, Times, LogData, OldState}};
allred(init_move_street,_From, StateData) ->
    io:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,    
    write_result(LogData, io_lib:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    {reply, {allred,Siblings},redgreenred, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}}.


%% Moving AVENUES       
greenredred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    update_lanes(ManagedLanes, [av], [ca,avf],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {next_state, greenredred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
greenredred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {next_state, allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, greenredred}};
greenredred(Event, StateData) ->
    unexpected_event(greenredred, Event, StateData),
    {next_state, greenredred, StateData}.
%%Synch calls
greenredred(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings, Times, LogData, OldState}) ->
    {reply, {greenredred,Times, Siblings,LogData, OldState},greenredred, StateData};
greenredred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {greenredred,DataLog},greenredred, {LightId,ManagedLanes, Siblings,Times, LogData, OldState}};
greenredred(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [av], [ca,avf],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {reply, {greenredred,Siblings},greenredred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
greenredred(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {greenredred,Siblings},allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, greenredred}}.

%% Moving STREETS    
redgreenred(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [ca], [av,avf],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {next_state, redgreenred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redgreenred(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {next_state, allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, redgreenred}};
redgreenred(Event, StateData) ->
    unexpected_event(redgreenred, Event, StateData),
    {next_state, redgreenred, StateData}.      
redgreenred(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings,Times, LogData, OldState}) ->
    {reply, {redgreenred,Times,Siblings, LogData, OldState},redgreenred, StateData};
redgreenred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {redgreenred,DataLog},redgreenred, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}};
redgreenred(move_street,_From, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [ca], [av,avf],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {reply, {redgreenred,Siblings},redgreenred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redgreenred(idle,_From, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings,Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca,av,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {redgreenred,Siblings},allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, redgreenred}}.
    
%% Moving OTHER AVENUES (LIGHT WITH to avenues different direction)
redredgreen(move_avenueF, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(AvF, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [avf], [ca,av],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {next_state, greenredred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redredgreen(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {next_state, allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, greenredred}};
redredgreen(Event, StateData) ->
    unexpected_event(greenredred, Event, StateData),
    {next_state, greenredred, StateData}.
%%Synch calls
redredgreen(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings, Times, LogData, OldState}) ->
    {reply, {greenredred,Times, Siblings,LogData, OldState},greenredred, StateData};
redredgreen({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {greenredred,DataLog},greenredred, {LightId,ManagedLanes, Siblings,Times, LogData, OldState}};
redredgreen(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(AvF, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [avf], [ca,av],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {reply, {greenredred,Siblings},greenredred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redredgreen(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(AvF, CTime, LogData),
    update_lanes(ManagedLanes, [], [av,ca,avf],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {greenredred,Siblings},allred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, greenredred}}.    
    
    
    
%% private functions
unexpected_event(_CurrentState, _Event, _StateData) ->
    io:format("traffic light error~n").
    
test() -> 
    {ok,final}.

evaluate_state({LightId, LightPid, Time}) ->
    io:format("Evaluating state ~w~n",[LightPid]),
   {State, Times, _Siblings, LogData, OldState} = get_state(LightPid),
   
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
 
evaluate_state(State, _OldState, NextTime, CTime, ARTime, ARTimer, LogData, LightPid) when NextTime >= CTime, ARTimer <  ARTime->
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
   

%%   if
%%       Next_time >= CTime ->           
%%	   
%%	   case State of             
%%	       greenredred -> move_street(LightPid);
%%	       redgreenred -> move_avenue(LightPid)
%%	   end;
%%       Next_time < CTime ->
%%	   io:format("Continuing ~w way cycle~n",[State]),
%%	   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
%%	   case State of
%%	       allred   -> estimate_after_idle(LightPid, LogData);
%%		           %%evaluate_state(LightPid);           
%%	       greenredred -> move_avenue(LightPid);
%%	       redgreenred -> move_street(LightPid)
%%           end;
%%       true ->
%%           idle(LightPid)
%%   end,
%%   io:format("-----------------------------------------------~n~n"),
%%   write_result(LogData, io_lib:format("----------------------------------------------------",[])).

%%%%%%
%%INPUT: ManagedLanes: all lanes connected to the light (av, ca, etc).
%%		 Active: key name (0 or 1) to find lanes to update on active
%%		 Idle:	 key names (1 or more) to find lanes to update on idle 
%%OUTPUT: None
%%DESC:	 This function is used to call all lanes and tell them to update either on idle or active

update_lanes(ManagedLanes, Active, Idle,CTime, GTime, LogData) ->
	update_lanes_active_aux(ManagedLanes, Active, CTime, GTime, LogData), 
    update_lanes_idle_aux(ManagedLanes, Idle, CTime, LogData).
	
update_lanes_active_aux(_ManagedLanes, [], _CTime, _GTime, _LogData)->
	[];
update_lanes_active_aux(ManagedLanes, [Active | Tail], CTime, GTime, LogData)->
	Res = lists:keyfind(Active, 1, ManagedLanes),
	case Res of
    	{Active, List} -> update_on_active(List, CTime, GTime, LogData);
    	false		   -> []		
    end,	
	update_lanes_active_aux(ManagedLanes, Tail, CTime, GTime, LogData).

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
update_on_active([], _Cycle_time, _Go_time, _LogData) ->  
    true;
update_on_active([{LaneId,LanePid}|Tail], Go_time, Cycle_time, LogData) ->  
    LanePid ! {go, self(), Cycle_time, Go_time, LogData},
    receive
        {reply, _Reply} -> 
            %%io:format("reply recieve after update on active lane ~w.~n",[LaneId]),
            write_result(LogData, io_lib:format("reply recieve after update on active lane ~w",[LaneId]))
    end,
    update_on_active(Tail,Cycle_time, Go_time, LogData).

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
