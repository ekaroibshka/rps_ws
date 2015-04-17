-module(session).





-include("rps_aliases.hrl").





-behaviour(gen_server).





-export([incoming_event/3, send/2]).
-export([init/1, start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).





incoming_event(Session, Event, Msg) -> gen_server:cast(Session, {Event, Msg}).





send(State, Msg) -> notify_ws(State, ws_send_text, Msg).

notify_ws(State, Event, Msg) -> ws:reply(?SS:get_ws(State), {Event, Msg}).





start_link(WS) -> gen_server:start_link(?MODULE, ?SS:default(WS), []).





init(State) ->
	Nick = register_self(),
	gproc:reg(?E:chat_broadcast()),
	gproc:reg(?E:user(Nick)),
	{ok, ?SS:new(State, ?SS:user(Nick))}.

register_self() ->
	Nick = gen_nick(8),
	case ?R:reg(Nick) of
		ok -> Nick;
		{error, already_exists} -> register_self()
	end.

gen_nick(Bytes) -> base64:encode(crypto:strong_rand_bytes(Bytes)).





handle_call(Msg, From, State) -> ?C:state_call(?SS:get_name(State), Msg, From, State).



handle_cast({ws_text_arrived, Text}, State) -> handle_text(Text, State);

handle_cast({ws_closed, Reason}, State) -> {stop, Reason, State};

handle_cast(Msg, State) -> ?C:state_cast(?SS:get_name(State), Msg, State).



handle_text(<<"/w "/utf8, Whisper/binary>>, State) -> 
 	[Nick, Text] = binary:split(Whisper, <<" ">>),
 	case ?SS:get_user(State) of
 		Nick ->
 			send(State, <<"You are not allowed to whisper yourself.\r\n"/utf8>>),
		 	{noreply, State};
 		_ ->
		 	?E:pub(?E:user(Nick), {private, ?SS:get_user(State), Text}),
		 	Me = ?SS:get_user(State),
		 	send(State, <<Me/binary, " -> "/utf8, Nick/binary, ": "/utf8, Text/binary>>),
		 	{noreply, State}
	end;

handle_text(<<"/"/utf8, Command/binary>>, State) -> 
	[Cmd | Args] = binary:split(Command, <<" "/utf8>>, [global]),
	?C:state_cmd(?SS:get_name(State), Cmd, Args, State);

handle_text(Text, State) -> 
	?E:pub(?E:chat_broadcast(), {broadcast, ?SS:get_user(State), Text}),
	{noreply, State}.



handle_info({broadcast, From, Msg}, State) -> 
	send(State, <<From/binary, ": "/utf8, Msg/binary>>),
	{noreply, State};

handle_info({private, From, Msg}, State) -> 
	Me = ?SS:get_user(State),
	send(State, <<From/binary, " -> "/utf8, Me/binary, ": "/utf8, Msg/binary>>),
	{noreply, State};

handle_info(_Info, State) -> {noreply, State}.





terminate(_Reason, State) ->
	?R:unreg(?SS:get_user(State)),
	?E:unsub(?E:chat_broadcast()),
	?E:unsub(?E:user(?SS:get_user(State))),
	case ?SS:get_judge(State) of
		undefined -> ok;
		Pid ->
			?J:quit_me(Pid, self()),
			ok
	end.
	




code_change(_OldVsn, State, _Extra) -> {ok, State}.