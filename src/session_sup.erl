-module(session_sup).





-include("rps_aliases.hrl").





-behaviour(supervisor).





-export([start_link/0, init/1, create_session/1]).





create_session(WSPid) -> supervisor:start_child(?SSUP, [WSPid]).





start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).





init([]) ->
    {
    	ok,
    	{
    		{simple_one_for_one, 0, 1},
          	[
          		?CHILD(?S, temporary, worker)
            ]
        }
    }.