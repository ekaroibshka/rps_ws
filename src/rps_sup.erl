-module(rps_sup).





-include("rps_aliases.hrl").





-behaviour(supervisor).





-export([start_link/0]).

-export([init/1]).





start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).





init([]) ->
	{
    	ok, {
    			{one_for_one, 10, 60},
    			[
    				?CHILD(?R, permanent, worker),
    				?CHILD(?SSUP, permanent, supervisor),
    				?CHILD(?JSUP, permanent, supervisor)
 				]
 			}
 	}.