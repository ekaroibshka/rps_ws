-define(CHILD(I, Kind, Type), {I, {I, start_link, []}, Kind, 5000, Type, [I]}).

-define(R, registry).
-define(RS, registry_state).

-define(S, session).
-define(SS, session_state).
-define(SSUP, session_sup).

-define(J, judge).
-define(JS, judge_state).
-define(JSUP, judge_sup).

-define(C, client).

-define(E, event).