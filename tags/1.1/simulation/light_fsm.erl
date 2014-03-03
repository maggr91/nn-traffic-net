-module(light_fsm).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         get_state/1, tabulate_data/2]).

%states
-export([ redred/2, redred/3, greenred/2, greenred/3, redgreen/2, redgreen/3]).
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
    %%{LightId, {Av, Ca},Siblings, Times, LogData} = Args,
    {LightId, ManagedLanes,Siblings, Times, LogData} = Args,
    file:delete(LogData), %% delete old log
    NewTimes = [{allred_timer, 0} | Times],
    {ok, redred,{LightId, ManagedLanes,Siblings, NewTimes, LogData, redred}}.

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
    {LightId, ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    {next_state, greenred, {LightId, ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redred(move_street, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),    
    {LightId, ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    %%{cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    {next_state, redgreen, {LightId, ManagedLanes, Siblings, NewTimes, LogData, OldState}};
redred(Event, StateData) ->
    unexpected_event(redred, Event, StateData),
    {next_state, redred, StateData}.

%%Synch calls
redred(get_state, _From, StateData = {_LightId, _ManagedLanes,Siblings, Times, LogData, OldState}) ->
    {reply, {redred,Times,Siblings, LogData, OldState},redred, StateData};
redred({update_siblings, Siblings},_From, StateData) ->
    %io:format("Updating StateData: ~w~n",[Siblings]),
    {LightId,ManagedLanes,_Siblings, Times, LogData, OldState} = StateData,
    {reply, {redred,Siblings},redred, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}};
redred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {redred,DataLog},redred, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}};
redred(move_avenue,_From, StateData) ->
    io:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    update_lanes(ManagedLanes, [], [av, ca],CTime, 0, LogData),
    NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    {reply, {redred,Siblings},greenred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redred(move_street,_From, StateData) ->
    io:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    write_result(LogData, io_lib:format("Changing for red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca, av],CTime, 0, LogData),
    NewTimesAux = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, NewTimesAux, {allred_timer, 0}),
    {reply, {redred,Siblings},redgreen, {LightId,ManagedLanes, Siblings, NewTimes, LogData, OldState}};
redred(idle,_From, StateData) ->
    io:format("All Red time. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings,Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("All Red time. Data: ~w~n",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca, av],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {redgreen,Siblings},redred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
    
redred(init_move_avenue,_From, StateData) ->
    io:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    write_result(LogData, io_lib:format("First move of lights from red to green on avenue. Start Moving avenue lanes. Data: ~w~n",[StateData])),
    {reply, {redred,Siblings},greenred, {LightId,ManagedLanes,Siblings, Times, LogData, OldState}};
redred(init_move_street,_From, StateData) ->
    io:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,    
    write_result(LogData, io_lib:format("First move of lights from red to green on streets. Start Moving street lanes. Data: ~w~n",[StateData])),
    {reply, {redred,Siblings},redgreen, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}}.


%% Moving AVENUES       
greenred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    update_lanes(ManagedLanes, [av], [ca],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {next_state, greenred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
greenred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    update_lanes(ManagedLanes, [], [av, ca],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {next_state, redred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, greenred}};
greenred(Event, StateData) ->
    unexpected_event(greenred, Event, StateData),
    {next_state, greenred, StateData}.
%%Synch calls
greenred(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings, Times, LogData, OldState}) ->
    {reply, {greenred,Times, Siblings,LogData, OldState},greenred, StateData};
greenred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {greenred,DataLog},greenred, {LightId,ManagedLanes, Siblings,Times, LogData, OldState}};
greenred(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_active(Av, CTime, GTime, LogData), 
    %%update_on_idle(Ca, CTime, LogData),
    update_lanes(ManagedLanes, [av], [ca],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {reply, {greenred,Siblings},greenred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
greenred(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    %%update_on_idle(Av, CTime, LogData),
    %%update_on_idle(Ca, CTime, LogData),
    update_lanes(ManagedLanes, [], [av, ca],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {greenred,Siblings},redred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, greenred}}.

%% Moving STREETS    
redgreen(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca, av],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {next_state, redgreen, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redgreen(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca, av],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {next_state, redred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, redgreen}};
redgreen(Event, StateData) ->
    unexpected_event(redgreen, Event, StateData),
    {next_state, redgreen, StateData}.      
redgreen(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings,Times, LogData, OldState}) ->
    {reply, {redgreen,Times,Siblings, LogData, OldState},redgreen, StateData};
redgreen({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,ManagedLanes, Siblings, Times, LogData, OldState} = StateData,
    write_final_data(ManagedLanes, DataLog),
    {reply, {redgreen,DataLog},redgreen, {LightId,ManagedLanes, Siblings, Times, LogData, OldState}};
redgreen(move_street,_From, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings, Times, LogData, OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {go_time, GTime} = lists:keyfind(go_time, 1, Times),
    write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    %%update_on_active(Ca, CTime, GTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [av], [ca],CTime, GTime, LogData),
    NewTimes = lists:keyreplace(go_time,1, Times, {go_time, GTime + 1}),
    {reply, {redgreen,Siblings},redgreen, {LightId,ManagedLanes,Siblings, NewTimes, LogData, OldState}};
redgreen(idle,_From, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {LightId,ManagedLanes,Siblings,Times, LogData, _OldState} = StateData,
    {cycle_time, CTime} = lists:keyfind(cycle_time, 1, Times),
    {allred_timer, ARTimer} = lists:keyfind(allred_timer, 1, Times),
    write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    %%update_on_idle(Ca, CTime, LogData),
    %%update_on_idle(Av, CTime, LogData),
    update_lanes(ManagedLanes, [], [ca,av],CTime, 0, LogData),
    %%NewTimes = lists:keyreplace(go_time,1, Times, {go_time, 0}),
    NewTimes = lists:keyreplace(allred_timer,1, Times, {allred_timer, ARTimer + 1}),
    {reply, {redgreen,Siblings},redred, {LightId,ManagedLanes,Siblings, NewTimes, LogData, redgreen}}.
    
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
   

%%   if
%%       Next_time >= CTime ->           
%%	   
%%	   case State of             
%%	       greenred -> move_street(LightPid);
%%	       redgreen -> move_avenue(LightPid)
%%	   end;
%%       Next_time < CTime ->
%%	   io:format("Continuing ~w way cycle~n",[State]),
%%	   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
%%	   case State of
%%	       redred   -> estimate_after_idle(LightPid, LogData);
%%		           %%evaluate_state(LightPid);           
%%	       greenred -> move_avenue(LightPid);
%%	       redgreen -> move_street(LightPid)
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