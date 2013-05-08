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
    %io:fwrite("gen_fsm called ~w:init(~w)~n", [?MODULE, Args]),
    %{ok, redred,#state{}}.
    {LightId, {Av, Ca},Siblings, Cycle_time, Go_time, LogData} = Args,
    file:delete(LogData), %% delete old log
    {ok, redred,{LightId, {Av, Ca},Siblings, Cycle_time, Go_time, LogData}}.

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
    io:format("Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId, {Av, Ca},Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("Moving avenue lanes. Data: ~w",[StateData])),
    update_on_idle(Av, Cycle_time, LogData),
    update_on_idle(Ca, Cycle_time, LogData),
    {next_state, greenred, {LightId, {Av, Ca},Siblings, Cycle_time, Go_time, LogData}};
redred(move_street, StateData) ->
    io:format("Moving street lanes. Data: ~w~n",[StateData]),    
    {LightId, {Av, Ca}, Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("Moving street lanes. Data: ~w",[StateData])),
    update_on_idle(Ca, Cycle_time, LogData),
    update_on_idle(Av, Cycle_time, LogData),
    {next_state, redgreen, {LightId, {Av, Ca}, Siblings, Cycle_time, Go_time, LogData}};
redred(Event, StateData) ->
    unexpected_event(redred, Event, StateData),
    {next_state, redred, StateData}.
%%Synch calls
redred(get_state, _From, StateData = {_LightId, _ManagedLanes,Siblings, Cycle_time, Go_time, LogData}) ->
    {reply, {redred,Cycle_time, Go_time,Siblings, LogData},redred, StateData};
redred({update_siblings, Siblings},_From, StateData) ->
    %io:format("Updating StateData: ~w~n",[Siblings]),
    {LightId,{Av, Ca},_Siblings, Cycle_time, Go_time, LogData} = StateData,
    {reply, {redred,Siblings},redred, {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData}};
redred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_final_data({Av, Ca}, DataLog),
    {reply, {redred,DataLog},redred, {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData}};
redred(move_avenue,_From, StateData) ->
    io:format("Moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("Moving avenue lanes. Data: ~w",[StateData])),
    update_on_idle(Av, Cycle_time, LogData),
    update_on_idle(Ca, Cycle_time, LogData),
    {reply, {redred,Siblings},greenred, {LightId,{Av, Ca},Siblings, Cycle_time, Go_time, LogData}};
redred(move_street,_From, StateData) ->
    io:format("Moving street lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("Moving street lanes. Data: ~w",[StateData])),
    update_on_idle(Ca, Cycle_time, LogData),
    update_on_idle(Av, Cycle_time, LogData),
    {reply, {redred,Siblings},redgreen, {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData}}.

%% Moving AVENUES       
greenred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    update_on_active(Av, Cycle_time, Go_time, LogData), 
    update_on_idle(Ca, Cycle_time, LogData),
    {next_state, greenred, {LightId,{Av, Ca},Siblings, Cycle_time, Go_time + 1, LogData}};
greenred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, _Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    update_on_idle(Av, Cycle_time, LogData),
    update_on_idle(Ca, Cycle_time, LogData),
    {next_state, redred, {LightId,{Av, Ca},Siblings, Cycle_time, 0, LogData}};
greenred(Event, StateData) ->
    unexpected_event(greenred, Event, StateData),
    {next_state, greenred, StateData}.
%%Synch calls
greenred(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings, Cycle_time, Go_time, LogData}) ->
    {reply, {greenred,Cycle_time, Go_time, Siblings,LogData},greenred, StateData};
greenred({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_final_data({Av, Ca}, DataLog),
    {reply, {greenred,DataLog},greenred, {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData}};
greenred(move_avenue, _From, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("continue moving avenue lanes. Data: ~w",[StateData])),
    update_on_active(Av, Cycle_time, Go_time, LogData), 
    update_on_idle(Ca, Cycle_time, LogData),
    {reply, {greenred,Siblings},greenred, {LightId,{Av, Ca},Siblings, Cycle_time, Go_time + 1, LogData}};
greenred(idle, _From, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, _Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("stop moving avenue lanes. Data: ~w",[StateData])),
    update_on_idle(Av, Cycle_time, LogData),
    update_on_idle(Ca, Cycle_time, LogData),
    {reply, {greenred,Siblings},redred, {LightId,{Av, Ca},Siblings, Cycle_time, 0, LogData}}.

%% Moving STREETS    
redgreen(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    update_on_active(Ca, Cycle_time, Go_time, LogData),
    update_on_idle(Av, Cycle_time, LogData),
    {next_state, redgreen, {LightId,{Av, Ca},Siblings, Cycle_time, Go_time + 1, LogData}};
redgreen(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, _Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    update_on_idle(Ca, Cycle_time, LogData),
    update_on_idle(Av, Cycle_time, LogData),
    {next_state, redred, {LightId,{Av, Ca},Siblings, Cycle_time, 0, LogData}};
redgreen(Event, StateData) ->
    unexpected_event(redgreen, Event, StateData),
    {next_state, redgreen, StateData}.      
redgreen(get_state, _From, StateData = {_LightId,_ManagedLanes,Siblings, Cycle_time, Go_time, LogData}) ->
    {reply, {redgreen,Cycle_time, Go_time,Siblings, LogData},redgreen, StateData};
redgreen({tabulate_data, DataLog},_From, StateData) ->
    io:format("Writing down data results: ~p~n",[DataLog]),
    {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_final_data({Av, Ca}, DataLog),
    {reply, {redgreen,DataLog},redgreen, {LightId,{Av, Ca}, Siblings, Cycle_time, Go_time, LogData}};
redgreen(move_street,_From, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("continue moving street lanes. Data: ~w",[StateData])),
    update_on_active(Ca, Cycle_time, Go_time, LogData),
    update_on_idle(Av, Cycle_time, LogData),
    {reply, {redgreen,Siblings},redgreen, {LightId,{Av, Ca},Siblings, Cycle_time, Go_time + 1, LogData}};
redgreen(idle,_From, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {LightId,{Av, Ca},Siblings, Cycle_time, _Go_time, LogData} = StateData,
    write_result(LogData, io_lib:format("stop moving street lanes. Data: ~w",[StateData])),
    update_on_idle(Ca, Cycle_time, LogData),
    update_on_idle(Av, Cycle_time, LogData),
    {reply, {redgreen,Siblings},redred, {LightId,{Av, Ca},Siblings, Cycle_time, 0, LogData}}.
    
%% private functions
unexpected_event(_CurrentState, _Event, _StateData) ->
    io:format("traffic light error~n").
    
test() -> 
    {ok,final}.
 
evaluate_state({LightId, LightPid, Time}) ->
   io:format("Evaluating state ~w~n",[LightPid]),
   {State, Cycle_time, Go_time, _Siblings, LogData} = get_state(LightPid),
   
   write_result(LogData, 
       io_lib:format("Running simulation iteration: ~w continue",[Time])),
   write_result(LogData, io_lib:format("Evaluating state for light_fsm: ~w continue",[LightId])),
   
   Next_time = Go_time + 1,
   if
       Next_time >= Cycle_time ->
           io:format("Finishing ~w way cycle moving to idle~n",[State]),
	   write_result(LogData, io_lib:format("Finishing ~w way cycle moving to idle",[State])),
	   idle(LightPid),		 
	   case State of             
	       greenred -> move_street(LightPid);
	       redgreen -> move_avenue(LightPid)
	   end;
       Next_time < Cycle_time ->
	   io:format("Continuing ~w way cycle~n",[State]),
	   write_result(LogData, io_lib:format("Continuing ~w way cycle",[State])),
	   case State of
	       redred   -> estimate_after_idle(LightPid, LogData);
		           %%evaluate_state(LightPid);           
	       greenred -> move_avenue(LightPid);
	       redgreen -> move_street(LightPid)
           end;
       true ->
           idle(LightPid)
   end,
   io:format("-----------------------------------------------~n~n"),
   write_result(LogData, io_lib:format("----------------------------------------------------",[])).

%% Update each lane that is active on this light 
update_on_active([], _Cycle_time, _Go_time, _LogData) ->  
    true;
update_on_active([{LaneId,LanePid}|Tail], Go_time, Cycle_time, LogData) ->  
    LanePid ! {go, self(), Cycle_time, Go_time, LogData},
    receive
        {reply, _Reply} -> 
            io:format("reply recieve after update on active lane ~w.~n",[LaneId]),
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
            io:format("reply recieve after update on idle lane ~w.~n",[LaneId]),
            write_result(LogData, io_lib:format("reply recieve after update on idle lane ~w",[LaneId]))
    end,
    update_on_idle(Tail, Cycle_time, LogData).
    
estimate_after_idle(LightPid, LogData) ->
    case random:uniform(2) of
        1 -> io:format("First moving avenues ~w~n",[LightPid]),
             write_result(LogData, io_lib:format("First moving avenues ~w",[LightPid])),
             move_avenue(LightPid),
             move_avenue(LightPid);
        2 -> io:format("First moving streets ~w~n",[LightPid]),
             write_result(LogData, io_lib:format("First moving streets ~w",[LightPid])),
             move_street(LightPid),
             move_street(LightPid)
    end.
    
%% Write down results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).
    
write_final_data({Av, Ca}, Path) ->
    write_final_data(Av, Path),
    write_final_data(Ca, Path);
    
write_final_data([], Path) ->
    write_result(Path, io_lib:format("FINISH WRITEDOWN FOR LANE",[])),
    write_result(Path, io_lib:format("=======================================",[]));
write_final_data([{LaneId,LanePid}|Tail], Path) ->
    write_result(Path, io_lib:format("=======================================",[])),
    write_result(Path, io_lib:format("START WRITEDOWN FOR LANE: ~w",[LaneId])),
    
    LanePid ! {write_down, self(), Path,LaneId},
    receive
        {reply, _Reply} -> 
            io:format("reply recieve after writedown lane ~w.~n",[LaneId])            
    end,
    write_final_data(Tail, Path).

