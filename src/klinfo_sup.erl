-module(klinfo_sup).

-behaviour(supervisor).

-include("global.hrl").

-export([start_link/0]).

-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
%%	WorkerSup = #{
%%		id => klinfo_worker_sup,
%%		start => {klinfo_worker_sup, start_link, []},
%%		restart => permanent,
%%		shutdown => infinity,
%%		type => supervisor,
%%		modules => [klinfo_worker_sup]
%%	},
	PoolerSup = #{
		id => pooler_sup,
		start => {pooler_sup, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [pooler_sup]
	},
	KlinfoEts = #{
		id => klinfo_ets,
		start => {klinfo_ets, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [klinfo_ets]
	},
	Children = [PoolerSup, KlinfoEts],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
