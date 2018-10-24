-module(klinfo_worker).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").

%% API
-export([start_link/0]).

-export([crud/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {

}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).


%%
%% Handles crud
%%
-spec crud(crud_verb(), message()) -> ok.
crud(CrudVerb, Message) ->
	case try_get_worker() of
		{ok, Worker} ->
			gen_server:call(Worker, {CrudVerb, Message});
		Error -> Error
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{}}.


handle_call({insert, {Table, Message}}, _From, State) ->
	Id = generate_uuid(Table),
	Reply = case ets:insert(Table, [{Id, Message}]) of
				true -> {ok, inserted};
				_ -> {error, insert_failed}
			end,
	{reply, Reply, State};
handle_call({read, {Table, Id, Key}}, _From, State) ->
	Reply = case ets:lookup(Table, Id) of
				[] -> {error, not_found};
				[{_, Map} | _] ->
					Value = maps:get(Key, Map),
					{ok, #{Key => Value}};
				_ -> {error, not_found}
			end,
	{reply, Reply, State};
handle_call({read_all, {Table, Id}}, _From, State) ->
	Reply = case ets:lookup(Table, Id) of
				[] -> {error, not_found};
				[{_, Map} | _] ->
					Map2 = Map#{<<"id">> => Id},
					{ok, Map2};
				_ -> {error, not_found}
			end,
	{reply, Reply, State};
handle_call({update_part, {Table, Id, Map}}, _From, State) ->
	Reply = case ets:lookup(Table, Id) of
				[] -> {error, not_found};
				[{_, Map} | _] ->

					case ets:insert(Table, [{Id, Map}]) of
						true -> {ok, updated};
						_ -> {error, update_failed}
					end;
				_ -> {error, not_found}
			end,
	{reply, Reply, State};
handle_call({update, {Table, Id, Map}}, _From, State) ->
	Reply = case ets:lookup(Table, Id) of
				[] -> {error, not_found};
				[{_, _TableMap} | _] ->
					case ets:insert(Table, [{Id, Map}]) of
						true -> {ok, updated};
						_ -> {error, update_failed}
					end;
				_ -> {error, not_found}
			end,
	{reply, Reply, State};
handle_call({delete, {Table, Id, Key}}, _From, State) ->
	Reply = case ets:lookup(Table, Id) of
				[] -> {error, not_found};
				[{_, Map} | _] ->
					Map2 = maps:update(Key, <<"">>, Map),
					case ets:insert(Table, [{Id, Map2}]) of
						true -> {ok, value_deleted};
						_ -> {error, delete_failed}
					end;
				_ -> {error, not_found}
			end,
	{reply, Reply, State};
handle_call({delete_all, {Table, Id}}, _From, State) ->
	Reply = case ets:lookup(Table, Id) of
				[] -> {error, not_found};
				[{_, _Map} | _] ->
					case ets:delete(Table, Id) of
						true -> {ok, deleted};
						_ -> {error, delete_failed}
					end;
				_ -> {error, not_found}
			end,
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State};
handle_info(_Info, State) ->
	{noreply, State}.


terminate(_, State) ->
	%% ?WARN("MONITOR: Monitor [~p] for node [~p] is stopped", [Node, self()]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% Tries to get a free worker (gs) process from klinfo (pool)
%% @private
%%
-spec(try_get_worker() -> {ok, Worker :: pid()} | {error, no_members_available}).
try_get_worker() ->
	try_get_worker(3).
try_get_worker(0) ->
	{error, no_members_available};
try_get_worker(N) ->
	case pooler:take_member(klinfo) of
		error_no_members ->
			%% ?WARN("POOLER (~p): No members available! Retrying [1/3]... ", [klinfo_pool]),
			try_get_worker(N - 1);
		Worker ->
			{ok, Worker}
	end.


%% @private
generate_uuid(Table) ->
	<<A:32, _B:16, _C:16, _D:16, _E:48>> = crypto:strong_rand_bytes(16),
	Uid = io_lib:format("~8.16.0b", [A]),
	TableBin = atom_to_binary(Table, latin1),
	UidBin = list_to_binary(Uid),
	<<TableBin/binary, "_", UidBin/binary>>.


%% @private
shorten_log_output(Output) when is_binary(Output) ->
	case byte_size(Output) of
		X when X > ?LOGGER_OUTPUT_MAX_LENGTH ->
			Bin = binary:part(Output, 0, ?LOGGER_OUTPUT_MAX_LENGTH),
			<<Bin/binary, "...">>;
		_ ->
			Output
	end.
