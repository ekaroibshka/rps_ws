-module(registry_state).





% MEMBER DATA CREATE WRAPS
-export([user/2]).

% OBJECT CREATION AND MANIPULATION
-export([default/0, reg_user/2, unreg_user/2]).

% MEMBER DATA GETTERS
-export([get_user/2, get_nicknames/1]).





% <-> SPEC REGISTRY_STATE -> GB TREE OF USER -> {Nick, Pid}




% -> API

% -> MEMBER DATA CREATE WRAPS

user(Nick, Pid) when is_binary(Nick) andalso is_pid(Pid) -> {Nick, Pid}.

% <- MEMBER DATA CREATE WRAPS





% -> OBJECT CREATION AND MANIPULATION

default() -> gb_trees:empty().

reg_user({Nick, Pid}, State) ->
	case gb_trees:is_defined(Nick, State) of
		true ->	{already_present, State};
		false -> 
			NewState = gb_trees:insert(Nick, Pid, State),
			{ok, NewState}
	end.

unreg_user(Nick, State) -> gb_trees:delete_any(Nick, State).
	
% <- OBJECT CREATION AND MANIPULATION





% -> MEMBER DATA GETTERS

get_nicknames(State) -> gb_trees:keys(State).

get_user(Nick, State) -> 
	case gb_trees:lookup(Nick, State) of
		none -> {error, no_such_user};
		{value, Pid} -> {ok, user(Nick, Pid)}
	end.

% <- MEMBER DATA GETTERS

% <- API