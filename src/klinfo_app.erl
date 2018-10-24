-module(klinfo_app).

-behaviour(application).

-include("global.hrl").

-export([start/0, start/2, stop/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start() ->
	_ = [application:start(Dep) || Dep <- resolve_deps(klinfo),
		not is_otp_base_app(Dep)],
	%% Cowboy settings
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/user/[:id]", klinfo_api, []},
			{"/task/[:id]", klinfo_api, []},
			{"/project/[:id]", klinfo_api, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, ?DEFAULT_PORT}], #{
		env => #{dispatch => Dispatch}
	}),
	%% Pooler settings
	PoolConfig = [{name, klinfo},
		{max_count, ?POOLER_MAX_PROCESSES},
		{init_count, ?POOLER_MIN_PROCESSES},
		{start_mfa, {klinfo_worker, start_link, []}}
	],
	pooler:new_pool(PoolConfig),
	ok.


start(_Type, _Args) ->
	klinfo_sup:start_link().


stop(_State) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

dep_apps(App) ->
	application:load(App),
	{ok, Apps} = application:get_key(App, applications),
	Apps.


all_deps(App, Deps) ->
	[[all_deps(Dep, [App | Deps]) || Dep <- dep_apps(App),
		not lists:member(Dep, Deps)], App].


resolve_deps(App) ->
	DepList = all_deps(App, []),
	{AppOrder, _} = lists:foldl(fun(A, {List, Set}) ->
		case sets:is_element(A, Set) of
			true ->
				{List, Set};
			false ->
				{List ++ [A], sets:add_element(A, Set)}
		end
								end,
		{[], sets:new()},
		lists:flatten(DepList)),
	AppOrder.


is_otp_base_app(kernel) ->
	true;
is_otp_base_app(stdlib) ->
	true;
is_otp_base_app(_) ->
	false.
