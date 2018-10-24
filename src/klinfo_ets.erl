-module(klinfo_ets).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").

%% API
-export([start_link/0]).

-export([]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(SERVER, ?MODULE).

-record(state, {

}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	erlang:send_after(200, self(), {init}),
	{ok, #state{}}.


handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({init}, State) ->
	ets:new(user, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(task, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(project, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
	{noreply, State};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State};
handle_info(_Info, State) ->
	{noreply, State}.


terminate(_, State) ->
	ets:delete(user),
	ets:delete(task),
	ets:delete(project),
	%% ?WARN("MONITOR: Monitor [~p] for node [~p] is stopped", [Node, self()]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
