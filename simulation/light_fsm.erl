-module(light_fsm).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         get_state/1]).

%states
-export([ redred/2, redred/3, greenred/2, greenred/3, redgreen/2, redgreen/3]).
%-record(state,{}).

%client calls
-export([start_link/1,move_avenue/1, move_street/1, idle/1,update_siblings/2, evaluate_state/1]).

%test
-export([test/0]).

%%client functions

move_avenue(Pid) ->
    gen_fsm:send_event(Pid,move_avenue).
    
move_street(Pid) ->
    gen_fsm:send_event(Pid,move_street).

idle(Pid) ->
    gen_fsm:send_event(Pid,idle).

update_siblings(Pid, Siblings) ->
    gen_fsm:sync_send_event(Pid,{update_siblings, Siblings}).
        
%% gen_fsm functions

start_link(Args) ->
    gen_fsm:start_link(?MODULE,Args,[]).

init(Args) ->
    io:fwrite("gen_fsm called ~w:init(~w)~n", [?MODULE, Args]),
    %{ok, redred,#state{}}.
    {ok, redred,Args}.

get_state(LightPid) ->
    try
        io:fwrite("Getting state ~w~n", [LightPid]),
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
    {{Av, _Ca},_Siblings, Cycle_time, _Go_time} = StateData,
    update_on_idle(Av, Cycle_time),
    {next_state, greenred, StateData};
redred(move_street, StateData) ->
    io:format("Moving street lanes. Data: ~w~n",[StateData]),
    {{_Av, Ca},_Siblings, Cycle_time, _Go_time} = StateData,
    update_on_idle(Ca, Cycle_time),
    {next_state, redgreen, StateData};
redred(Event, StateData) ->
    unexpected_event(redred, Event, StateData),
    {next_state, redred, StateData}.
redred(get_state, _From, StateData = {_ManagedLanes,Siblings, Cycle_time, Go_time}) ->
    {reply, {redred,Cycle_time, Go_time,Siblings},redred, StateData};
redred({update_siblings, Siblings},_From, StateData) ->
    io:format("Updating StateData: ~w~n",[Siblings]),
    {{Av, Ca},_Siblings, Cycle_time, Go_time} = StateData,
    {reply, {redred,Siblings},redred, {{Av, Ca}, Siblings, Cycle_time, Go_time}}.

%% Moving AVENUES       
greenred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {{Av, Ac},Siblings, Cycle_time, Go_time} = StateData,
    update_on_active(Av, Cycle_time, Go_time), 
    {next_state, greenred, {{Av, Ac},Siblings, Cycle_time, Go_time + 1}};
greenred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {{Av, Ca},Siblings, Cycle_time, _Go_time} = StateData,
    update_on_idle(Av, Cycle_time),
    {next_state, redred, {{Av, Ca},Siblings, Cycle_time, 0}};
greenred(Event, StateData) ->
    unexpected_event(greenred, Event, StateData),
    {next_state, greenred, StateData}.
greenred(get_state, _From, StateData = {_ManagedLanes,Siblings, Cycle_time, Go_time}) ->
    {reply, {greenred,Cycle_time, Go_time, Siblings},greenred, StateData}.

%% Moving STREETS    
redgreen(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {{Av, Ca},Siblings, Cycle_time, Go_time} = StateData,
    update_on_active(Ca, Cycle_time, Go_time),
    {next_state, redgreen, {{Av, Ca},Siblings, Cycle_time, Go_time + 1}};
redgreen(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {{Av, Ca},Siblings, Cycle_time, _Go_time} = StateData,
    update_on_idle(Ca, Cycle_time),
    {next_state, redred, {{Av, Ca},Siblings, Cycle_time, 0}};
redgreen(Event, StateData) ->
    unexpected_event(redgreen, Event, StateData),
    {next_state, redgreen, StateData}.      
redgreen(get_state, _From, StateData = {_ManagedLanes,Siblings, Cycle_time, Go_time}) ->
    {reply, {redgreen,Cycle_time, Go_time,Siblings},redgreen, StateData}.
    
%% private functions
unexpected_event(_CurrentState, _Event, _StateData) ->
    io:format("traffic light error~n").
    
test() -> 
    {ok,final}.
 
evaluate_state(LightId) ->
   io:format("Evaluating state ~w~n",[LightId]),
   {State, Cycle_time, Go_time, _Siblings} = get_state(LightId),
   Next_time = Go_time + 1,
   if
     Next_time >= Cycle_time ->
         io:format("Finishing ~w way cycle moving to idle~n",[State]),
         idle(LightId),
         io:format("Changing to ~w way cycle from idle~n",[State]),
	 case State of             
	     greenred -> move_street(LightId);
	     redgreen -> move_avenue(LightId)
	 end;
     Next_time < Cycle_time ->
         io:format("Continuing ~w way cycle~n",[State]),
	 case State of
	     redred   -> estimate_after_idle(LightId);
	      		 %%evaluate_state(LightId);           
	     greenred -> move_avenue(LightId);
	     redgreen -> move_street(LightId)
	 end;
   true ->
	 idle(LightId)
   end,
   io:format("-----------------------------------------------~n~n").

%% Update each lane that is active on this light 
update_on_active([], _Cycle_time, _Go_time) ->  
    [];
update_on_active([LaneId|Tail], Go_time, Cycle_time) ->  
    LaneId ! {go, self(), Cycle_time, Go_time},
    receive
        {reply, _Reply} -> io:format("reply recieve after update on active lane ~n")
    end,
    update_on_active(Tail,Cycle_time, Go_time).

%% Update each lane that is waiting on this light 
update_on_idle([], _Cycle_time) -> 
    [];
update_on_idle([LaneId|Tail], Cycle_time) ->
    LaneId ! {waiting, self(), Cycle_time},
    receive
        {reply, _Reply} -> io:format("reply recieve after update on idle lane ~n")
    end,
    update_on_idle(Tail, Cycle_time).
    
estimate_after_idle(LightPid) ->
    case random:uniform(2) of
        1 -> io:format("First moving avenues ~w~n",[LightPid]),
             move_avenue(LightPid),
             move_avenue(LightPid);
        2 -> io:format("First moving streets ~w~n",[LightPid]),
             move_street(LightPid),
             move_street(LightPid)
    end.
