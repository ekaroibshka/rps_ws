-module(session_state).





% MEMBER DATA CREATE WRAPS
-export([ws/1, name/1, user/1, judge/1]).

% OBJECT CREATION AND MANIPULATION
-export([new/4, new/2, default/1]).

% MEMBER DATA GETTERS
-export([get_ws/1, get_name/1, get_user/1, get_judge/1]).





% <-> SPEC  RPS_CONNECT_STATE -> PROPLIST -> [ws, status, user, judge]





% -> PUBLIC

% -> MEMBER DATA CREATE WRAPS

ws(WSPid) when is_pid(WSPid) -> {ws, WSPid}.

name(Name) when is_atom(Name) -> {name, Name};

name(ComplexName) when is_tuple(ComplexName) -> {name, ComplexName}.

user(UserName) when is_atom(UserName) orelse is_binary(UserName) -> {user, UserName}.

judge(JudgePid) when is_atom(JudgePid) orelse is_pid(JudgePid) -> {judge, JudgePid}.

% <- MEMBER DATA CREATE WRAPS





% -> OBJECT CREATION AND MANIPULATION

new(WSPid, Name, UserName, JudgePid) ->
	[
		ws(WSPid),
		name(Name),
		user(UserName),
		judge(JudgePid)
	].

new(NewState, []) -> NewState;

new(State, [Parameter | Rest]) -> new(replace(State, Parameter), Rest);
 
new(State, Parameter) -> replace(State, Parameter).

default(WSPid) -> new(WSPid, idle, undefined, undefined).

% <- OBJECT CREATION AND MANIPULATION





% -> MEMBER DATA GETTERS

get_ws(State) -> get_parameter(ws, State).

get_name(State) -> get_parameter(name, State).

get_user(State) -> get_parameter(user, State).

get_judge(State) -> get_parameter(judge, State).

% <- MEMBER DATA GETTERS

% <- PUBLIC





% -> PRIVATE

param_name({X, _}) -> X.

param_val({_, X}) -> X.


replace(_State = [ {ws, WSPid}, {name, Name}, {user, UserName}, {judge, JudgePid}], Parameter) ->
	case param_name(Parameter) of
		ws -> new(param_val(Parameter), Name, UserName, JudgePid);
		name -> new(WSPid, param_val(Parameter), UserName, JudgePid);
		user -> new(WSPid, Name, param_val(Parameter), JudgePid);
		judge -> new(WSPid, Name, UserName, param_val(Parameter));
		_ -> 'ERROR'
	end.


get_parameter(Parameter, State) ->
	{Parameter, Value} = proplists:lookup(Parameter, State),
	Value.

% <- PRIVATE