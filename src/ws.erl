-module(ws).





-include("rps_aliases.hrl").





% API (OUTER CALLS)
-export([init/2, reply/2]).

% WS_HANDLER CALLBACKS
-export([websocket_handle/3, websocket_info/3, terminate/3]).




% -> API (OUTER CALLS)

reply(WSPid, Message) -> WSPid ! Message.
    


init(Req, _Opts) -> 
    prepare_delayed_init(),
    {cowboy_websocket, Req, no_session}.

% <- API (OUTER CALLS)





% -> WS_HANDLER CALLBACKS

websocket_handle({text, MessageFromClient}, Req, State) ->
	notify_session(State, ws_text_arrived, MessageFromClient),
	{ok, Req, State};

websocket_handle(_MessageFromClient, Req, State) ->
	{ok, Req, State}.





websocket_info(delayed_init, Req, no_session) -> delayed_init(Req, no_session);

websocket_info({ws_send_text, Msg}, Req, State) -> {reply, {text, Msg}, Req, State};	

websocket_info(_Data, Req, State) -> {ok, Req, State}.





terminate(Reason, _Req, State) ->
	case State of
		no_session -> ok;
		_ -> 
			notify_session(State, ws_closed, Reason),
			ok
	end.

% <- WS_HANDLER CALLBACKS





% -> PRIVATE

prepare_delayed_init() -> self() ! delayed_init.

delayed_init(Req, _) ->	
	Session = create_session(self()),
	{ok, Req, Session}.

create_session(WSPid) ->
	{ok, Session} = ?SSUP:create_session(WSPid),
	Session.

notify_session(Session, Event, Msg) -> ?S:incoming_event(Session, Event, Msg).

% <- PRIVATE