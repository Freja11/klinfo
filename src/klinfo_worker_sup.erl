-module(klinfo_worker_sup).

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
%%	KlinforWorker = #{
%%		id => klinfo_worker,
%%		start => {klinfo_worker, start_link, []},
%%		restart => permanent,
%%		shutdown => infinity,
%%		type => worker,
%%		modules => [klinfo_worker]
%%	},
	Children = [],
	RestartStrategy = {simple_one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
