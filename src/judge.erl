-module(judge).



-include("rps_aliases.hrl").



-behaviour(gen_server).



-export([go/1, new/1, start_link/1, answer/3, quit_me/2]).

-export([init/1]).

-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% API

new(JS) -> supervisor:start_child(?JSUP, [JS]).



start_link(UL) -> gen_server:start_link(?MODULE, ?JS:new(UL), []).


go(Pid) -> gen_server:cast(Pid, go).


answer(Pid, SPid, Answer) -> gen_server:cast(Pid, {answer, SPid, Answer}).


quit_me(Pid, SPid) -> gen_server:cast(Pid, {quit_me, SPid}).




init(State) -> {ok, State}.


%handle_cast(go, State) -> 
%handle_cast({answer, Nick, Answer}, State) ->
%handle_cast({quit_me, Nick}, State) ->
%
%

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(go, State) -> 
	[U1, U2] = State,
	case ?C:start_game_req(?JS:get_pid(U1)) of
		'NO' ->
			?C:error(?JS:get_pid(U1)),
			?C:error(?JS:get_pid(U2)),
			{stop, normal, State};
		ok ->
			case ?C:start_game_req(?JS:get_pid(U2)) of
				'NO' ->
					?C:error(?JS:get_pid(U1)),
					?C:error(?JS:get_pid(U2)),
					{stop, normal, State};
				ok ->
					{noreply, State}
			end
	end;
	

handle_cast({quit_me, Pid}, State) ->
	[User1, User2] = State,
	case ?JS:get_user(State, Pid) of
		User1 ->
			?C:error(?JS:get_pid(User2)),
			{stop, normal, State};
		User2 ->
			?C:error(?JS:get_pid(User1)),
			{stop, normal, State}
  	end;

handle_cast({answer, SPid, Answer}, State) ->
	NewState = ?JS:set_user_answer(State, SPid, Answer),
	case ?JS:is_complete(NewState) of
		true ->	
			Res = ?JS:winner(NewState),
			[U1, U2] = NewState,
			?C:game_end_req(?JS:get_pid(U1), Res),
			?C:game_end_req(?JS:get_pid(U2), Res),
			{stop, normal, NewState};
		false -> {noreply, NewState}
	end;

handle_cast(_Request, State) -> {noreply, State}.



handle_info(_Info, State) -> {noreply, State}.



terminate(_Reason, _State) -> ok.



code_change(_OldVsn, State, _Extra) -> {ok, State}.
