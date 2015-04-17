-module(event).


-export([pub/2, sub/1, unsub/1, chat_broadcast/0, user/1]).


sub(Event) -> gproc:reg(Event).

unsub(Event) -> gproc:unreg(Event).

pub(Event, Msg) -> gproc:send(Event, Msg).

chat_broadcast() -> {p, l, chat_broadcast}.

user(Nick) -> {p, l, Nick}.