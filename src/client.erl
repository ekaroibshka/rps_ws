-module(client).

-include("rps_aliases.hrl").

-export([state_cast/3, state_call/4, state_cmd/4, start_game_req/1, error/1, game_end_req/2]).



challenge(State, Pid) -> gen_server:call(Pid, {i_challenge_you, ?SS:get_user(State)}).
accept_challenge(Pid) -> gen_server:call(Pid, challenge_accepted).
decline_challenge(Pid) -> gen_server:cast(Pid, challenge_declined).
reply_judge(State, Reply) -> ?J:answer(?SS:get_judge(State), self(), Reply).
start_game_req(Pid) -> gen_server:call(Pid, start_game).
game_end_req(Pid, Results) -> gen_server:cast(Pid, Results).
error(Pid) -> gen_server:cast(Pid, error).


state_cast(game_results, {winner, Pid}, State) -> 
	case self() of
		Pid ->
			?S:send(State, <<"Congratulations, you won!\r\n"/utf8>>),
			{noreply, ?SS:new(State, [?SS:name(idle), ?SS:judge(undefined)])};
		no_one ->
			?S:send(State, <<"Woah, it's draw\r\n"/utf8>>),
			{noreply, ?SS:new(State, [?SS:name(idle), ?SS:judge(undefined)])};
		_ -> 
			?S:send(State, <<"Sorry, you failed\r\n"/utf8>>),
			{noreply, ?SS:new(State, [?SS:name(idle), ?SS:judge(undefined)])}
	end;
	

state_cast(game_results, error, State) -> 
	?S:send(State, <<"Error occured, game results is undefined, returning to lobby\r\n"/utf8>>),
	{noreply, ?SS:new(State, [?SS:name(idle), ?SS:judge(undefined)])};

state_cast(game, error, State) -> 
	?S:send(State, <<"Error occured, game won't start, returning to lobby\r\n"/utf8>>),
	{noreply, ?SS:new(State, [?SS:name(idle), ?SS:judge(undefined)])};	

state_cast({challenging, _Pid}, challenge_declined, State) -> 
	?S:send(State, <<"Sorry, this user refuses to fight you\r\n"/utf8>>),
	{noreply, ?SS:new(State, ?SS:name(idle))};

state_cast(_, challenge_declined, State) -> 
	{noreply, State};

state_cast(_StateName, _Msg, State) -> {noreply, State}.



state_call(game_awaiting, start_game, _, State) -> 
	?S:send(State, <<"Game starts now!\r\nType your choice (/r,/p,/s)\r\n"/utf8>>),
	{reply, ok, ?SS:new(State, ?SS:name(game))};

state_call(_, start_game, _, State) -> 
	{reply, 'NO', State};
	
state_call({challenging, Pid}, challenge_accepted, {Pid, _}, State) -> 
	case ?J:new([self(), Pid]) of
		{ok, JPid} ->
			?S:send(State, <<"User accepted your challenge, your game will start soon\r\n"/utf8>>),
			{reply, {ok, JPid}, ?SS:new(State, [?SS:judge(JPid), ?SS:name(game_awaiting)])};
		_ ->
			?S:send(State, <<"User accepted your challenge, but an error occured; returning to lobby\r\n"/utf8>>),
			{reply, error, ?SS:new(State, ?SS:name(idle))}
	end;

state_call(_, challenge_accepted, _, State) -> 
	{reply, busy, State};

state_call(idle, {i_challenge_you, Nick}, {Pid, _}, State) -> 
	?S:send(State, <<"User \""/utf8, Nick/binary, "\", challenges you\r\n"/utf8>>),
	{reply, ok, ?SS:new(State, ?SS:name({challenged, Pid}))};

state_call(_, {i_challenge_you, _Pid}, _From, State) -> {reply, busy, State};

state_call(_StateName, _Msg, _From, State) -> {reply, ok, State}.





wrap_user_list(State, UL) -> 
	UserName = ?SS:get_user(State),
	Header = <<"==User list== \r\n"/utf8>>,
	Body = lists:foldl(	fun(X, Res) ->
							case X of
								UserName -> <<"\""/utf8, X/binary, "\" => YOU \r\n"/utf8, Res/binary>> ;
								_ -> <<"\""/utf8, X/binary, "\" \r\n"/utf8, Res/binary>> 
							end
						end, 
						<<"">>,
						UL),
	Footer = <<"=====End===== \r\n"/utf8>>,
	<<Header/binary, Body/binary, Footer/binary>>.

validate_nick(_Nick) -> valid.

state_cmd(_, <<"users"/utf8>>, _, State) -> 
	UserList = ?R:users(),
	?S:send(State, wrap_user_list(State, UserList)),
	{noreply, State};

state_cmd(_, <<"exit"/utf8>>, _, State) -> 
	?S:send(State, <<"Bye!\r\n"/utf8>>),
	{stop, exited_by_cmd, State};

% state_cmd(idle, Cmd, [NewNick | _], State) when Cmd == <<"sn"/utf8>> orelse Cmd == <<"setnick"/utf8>> -> 
% 	case validate_nick(NewNick) of
% 		valid ->
% 			case ?R:reg(NewNick) of
% 				ok -> 
% 					?R:unreg(?SS:get_user(State)),
% 					?E:unsub(?E:user(?SS:get_user(State))),
% 					?E:sub(?E:user(NewNick)),
% 					?S:send(State, <<"Nickname successfully changed.\r\n"/utf8>>),
% 					{noreply, ?SS:new(State, ?SS:user(NewNick))};
% 				{error, already_exists} -> 
% 					?S:send(State, <<"Such nickname already exists!\r\n"/utf8>>),
% 					{noreply, State}
% 			end;
% 		notvalid ->
% 			?S:send(State, <<"Incorrect nickname!\r\n"/utf8>>),
% 			{noreply, State}	
% 	end;

state_cmd(idle, Cmd, [ Nick | _], State) when Cmd == <<"c"/utf8>> orelse Cmd == <<"challenge"/utf8>> -> 
	case ?SS:get_user(State) of
		Nick -> 
			?S:send(State, <<"You are not allowed to challenge yourself\r\n"/utf8>>),
			{noreply, State};
		_ ->
			case ?R:find_user(Nick) of
				{ok, Pid} ->
					case challenge(State, Pid) of
						ok ->
							?S:send(State, <<"Let's wait user's answer\r\n"/utf8>>),
							{noreply, ?SS:new(State, ?SS:name({challenging, Pid}))};
						_ -> 
							?S:send(State, <<"Sorry, this user is currently busy\r\n"/utf8>>),
							{noreply, State}
					end;
				{error, no_such_user} -> 
					?S:send(State, <<"Sorry, no such user\r\n"/utf8>>),
					{noreply, State}
			end
		end;

state_cmd({challenged, Pid}, <<"accept"/utf8>>, _Args, State) -> 
	case accept_challenge(Pid) of
		{ok, JPid} -> 
			?S:send(State, <<"Your game will start soon\r\n"/utf8>>),
			?J:go(JPid),
			{noreply, ?SS:new(State, [?SS:judge(JPid), ?SS:name(game_awaiting)])};
		_ ->
			?S:send(State, <<"Error occured, returning to lobby\r\n"/utf8>>),
			{noreply, ?SS:new(State, ?SS:name(idle))}
	end;
	
state_cmd({challenged, Pid}, <<"deny"/utf8>>, _Args, State) -> 
	decline_challenge(Pid),
	?S:send(State, <<"Challenge declined\r\n"/utf8>>),
	{noreply, ?SS:new(State, ?SS:name(idle))};

state_cmd(game, Cmd, _Args, State) when Cmd == <<"r"/utf8>> orelse Cmd == <<"rock"/utf8>> -> 
	reply_judge(State, r),
	?S:send(State, <<"Await game results\r\n"/utf8>>),
	{noreply, ?SS:new(State, ?SS:name(game_results))};
state_cmd(game, Cmd, _Args, State) when Cmd == <<"p"/utf8>> orelse Cmd == <<"paper"/utf8>> -> 
	reply_judge(State, p),
	?S:send(State, <<"Await game results\r\n"/utf8>>),
	{noreply, ?SS:new(State, ?SS:name(game_results))};
state_cmd(game, Cmd, _Args, State) when Cmd == <<"s"/utf8>> orelse Cmd == <<"scissors"/utf8>> ->
	reply_judge(State, s),
	?S:send(State, <<"Await game results\r\n"/utf8>>),
	{noreply, ?SS:new(State, ?SS:name(game_results))};
	
state_cmd(_StateName, _Cmd, _Args, State) -> {noreply, State}.

