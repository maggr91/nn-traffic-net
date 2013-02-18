-module(light_fsm).
-behaviour(gen_fsm).

%gen_fsm behavior implementation
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states
-export([ redred/2, greenred/2, redgreen/2]).
-record(state,{}).

%client calls
-export([start_link/0,move_avenue/1, move_street/1, idle/1]).

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

start_link() ->
    gen_fsm:start_link(?MODULE,[],[]).

init([]) ->
    io:fwrite("gen_fsm called ~w:init(~w)~n", [?MODULE, redred]),
    {ok, redred,#state{}}.

handle_event(shutdown, _StateName, StateData) ->
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    io:fwrite("gen_fsm called ~w:handle_event(~w, ~w, ~w)~n",
        [?MODULE, Event, StateName, StateData]),
    {next_state, StateName, StateData}.
    
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
    
%% function
redred(move_avenue, StateData) ->
    io:format("Moving avenue lanes~n",[]),
    {next_state, greenred, StateData};
redred(move_street, StateData) ->
    io:format("Moving street lanes~n",[]),
    {next_state, redgreen, StateData};
redred(Event, StateData) ->
    unexpected_event(redred, Event, StateData),
    {next_state, redred, StateData}.
    
greenred(move_avenue, StateData) ->
    io:format("continue moving avenue lanes~n",[]),
    {next_state, greenred, StateData};
greenred(idle, StateData) ->
    io:format("stop moving avenue lanes~n",[]),
    {next_state, redred, StateData};
greenred(Event, StateData) ->
    unexpected_event(greenred, Event, StateData),
    {next_state, greenred, StateData}.
    
redgreen(move_street, StateData) ->
    io:format("continue moving street lanes~n",[]),
    {next_state, redgreen, StateData};
redgreen(idle, StateData) ->
    io:format("stop moving street lanes~n",[]),
    {next_state, redred, StateData};
redgreen(Event, StateData) ->
    unexpected_event(redgreen, Event, StateData),
    {next_state, redgreen, StateData}.      
    
%% private functions
unexpected_event(_CurrentState, _Event, _StateData) ->
    io:format("traffic light error~n").
    
test() -> 
    {ok,final}.
