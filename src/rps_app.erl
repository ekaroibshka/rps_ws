-module(rps_app).





-behaviour(application).





-export([start/2, stop/1]).





start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {file, "priv/index.html"}},
			{"/websocket", ws, []},
			{"/static/[...]", cowboy_static, {dir, "priv/static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
	[{env, [{dispatch, Dispatch}]}]),
    rps_sup:start_link().





stop(_State) ->
    ok.