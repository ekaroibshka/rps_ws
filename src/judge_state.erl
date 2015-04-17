-module(judge_state).





-export([new/1, get_user/2, get_pid/1, get_user_answer/2, get_answer/1, is_complete/1, set_user_answer/3, winner/1]).





% <-> SPEC RPS_JUDGE_STATE -> PROPLIST -> [{Pid, Answer}]





new(UserList) -> 
	lists:map(
		fun(X) ->
			user(X, undefined)
		end,
		UserList
	).



user(Pid, Answer) -> {Pid, Answer}.



get_user(State, Pid) ->
	case State of
		[A = {Pid, _}, _] -> A;
		[_, B = {Pid, _}] -> B;
		_ -> 'ERROR'
	end.

get_pid(User) -> get_user_parameter(User, pid).

get_user_answer(State, Param) -> get_answer(get_user(State, Param)).

get_answer(User) -> get_user_parameter(User, answer).

get_user_parameter(User, Parameter) ->
 {Pid, Answer} = User,
 case Parameter of
 	pid -> Pid;
 	answer -> Answer
 end.



is_complete(_State = [{_, A1},{_, A2}]) when (A1 == undefined) orelse (A2 == undefined) -> false;
is_complete(_) -> true.		

winner(State) ->
	[{U1Pid, User1Answer}, {U2Pid, User2Answer}] = State,
	case who_win(User1Answer, User2Answer) of
		first -> {winner, U1Pid};
		second -> {winner, U2Pid};
		stalemate -> {winner, no_one}
	end.

who_win(r, r) -> stalemate;
who_win(r, p) -> second;
who_win(r, s) -> first;

who_win(p, p) -> stalemate;
who_win(p, r) -> first;
who_win(p, s) -> second;

who_win(s, s) -> stalemate;
who_win(s, p) -> first;
who_win(s, r) -> second.


set_answer(User, Answer) ->
	{Pid, _} = User,
	user(Pid, Answer).

set_user_answer(State, UserParam, Answer) ->
	[U1, U2] = State,
	case get_user(State, UserParam) of
		U1 ->
			NewU1 = set_answer(U1, Answer),
			[NewU1, U2];
		U2 ->
			NewU2 = set_answer(U2, Answer),
			[U1, NewU2]
	end.
