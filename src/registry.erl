-module(registry).





-include("rps_aliases.hrl").





-behaviour(gen_server).





% API (OUTER CALLS)
-export([start_link/0, reg/1, unreg/1, users/0, find_user/1]).

% GEN_SERVER CALLBACKS
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).





% -> API (OUTER CALLS)

find_user(Nick) -> gen_server:call(?MODULE, {lookup, Nick}).

users() -> gen_server:call(?MODULE, userlist).

reg(Nick) -> gen_server:call(?MODULE, {reg, Nick}).

unreg(Nick) -> gen_server:cast(?MODULE, {unreg, Nick}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% <- API (OUTER CALLS)



% -> GEN_SERVER CALLBACKS

init(_Args) ->
  io:format("Global registry(~p) started!~n", [self()]),
  {ok, ?RS:default()}.



handle_call(userlist, _From, State) -> {reply, ?RS:get_nicknames(State), State};

handle_call({lookup, Nick}, _From, State) ->
  case ?RS:get_user(Nick, State) of
    {ok, {Nick, Pid}} -> {reply, {ok, Pid}, State};
    {error, no_such_user} -> {reply, {error, no_such_user}, State}
  end;

handle_call({reg, Nick}, _From = {Pid, _Ref}, State) ->
  case ?RS:reg_user(?RS:user(Nick, Pid), State) of
    {ok, NewState} -> {reply, ok, NewState};
    {already_present, State} -> {reply, {error, already_exists}, State}
  end;

handle_call(Request, _From, State) ->
  io:format("Global registry (~p) received unexpected call message: ~p~n", [self(), Request]),
  {reply, ok, State}.



handle_cast({unreg, Nick}, State) ->
  NewState = ?RS:unreg_user(Nick, State),
  {noreply, NewState};

handle_cast(Request, State) ->
  io:format("Global registry (~p) received unexpected cast message: ~p~n", [self(), Request]),
  {noreply, State}.



handle_info(Info, State) ->
  io:format("Global registry (~p) received unexpected message: ~p~n", [self(), Info]),
  {noreply, State}.



terminate(Reason, State) -> io:format("Global registry (~p) terminated: ~p, ~p~n",[?MODULE, Reason, State]).



code_change(_OldVsn, State, _Extra) -> {ok, State}.

% <- GEN_SERVER CALLBACKS