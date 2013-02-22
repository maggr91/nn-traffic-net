-module(light_fsm).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         get_state/1]).

%states
-export([ redred/2, redred/3, greenred/2, greenred/3, redgreen/2, redgreen/3]).
%-record(state,{}).

%client calls
-export([start_link/1,move_avenue/1, move_street/1, idle/1]).

%test
-export([test/0]).

%%client functions

move_avenue(Pid) ->
    gen_fsm:send_event(Pid,move_avenue).
    
move_street(Pid) ->
    gen_fsm:send_event(Pid,move_street).

idle(Pid) ->
    gen_fsm:send_event(Pid,idle).
    
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
    {next_state, greenred, StateData};
redred(move_street, StateData) ->
    io:format("Moving street lanes. Data: ~w~n",[StateData]),
    {next_state, redgreen, StateData};
redred(Event, StateData) ->
    unexpected_event(redred, Event, StateData),
    {next_state, redred, StateData}.
redred(get_state, _From, StateData = {_ManagedLanes,_State, On_time, Go_time}) ->
    {reply, {redred,On_time, Go_time},redred, StateData}.

%% Moving AVENUES       
greenred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes. Data: ~w~n",[StateData]),
    {{Av, Ac},_State, On_time, Go_time} = StateData,
    [Head|Tail] = Av,
    Head ! {test, self(), "prueba de que sirva"},
    receive
        {reply, Reply} -> io:format("ANSWER FROM LANE: ~w~n",[Reply])
    end,
    {next_state, greenred, StateData};
greenred(idle, StateData) ->
    io:format("stop moving avenue lanes. Data: ~w~n",[StateData]),
    {next_state, redred, StateData};
greenred(Event, StateData) ->
    unexpected_event(greenred, Event, StateData),
    {next_state, greenred, StateData}.
greenred(get_state, _From, StateData = {_ManagedLanes,_State, On_time, Go_time}) ->
    {reply, {greenred,On_time, Go_time},greenred, StateData}.

%% Moving STREETS    
redgreen(move_street, StateData) ->
    io:format("continue moving street lanes. Data: ~w~n",[StateData]),
    {next_state, redgreen, StateData};
redgreen(idle, StateData) ->
    io:format("stop moving street lanes. Data: ~w~n",[StateData]),
    {next_state, redred, StateData};
redgreen(Event, StateData) ->
    unexpected_event(redgreen, Event, StateData),
    {next_state, redgreen, StateData}.      
redgreen(get_state, _From, StateData = {_ManagedLanes,_State, On_time, Go_time}) ->
    {reply, {redgreen,On_time, Go_time},redgreen, StateData}.
    
%% private functions
unexpected_event(_CurrentState, _Event, _StateData) ->
    io:format("traffic light error~n").
    
test() -> 
    {ok,final}.