-module(klinfo_utils).

%% API
-export([echo1/2]).

-export([maps_get/2, maps_get/3, maps_put/3, maps_remove/2]).

-export([iso_timestamp/1, tdiff_seconds/2]).

-export([try_ets_lookup/2, try_ets_lookup/3]).

-type map_path() :: [binary() | atom()] | binary() | atom().
-type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type tid() :: integer().


%%%%%%%%%%%%%%%%%
%%%    API    %%%
%%%%%%%%%%%%%%%%%

-spec echo1(iolist(), iolist()) -> ok.
echo1(NameArg1, Arg1) ->
	io:format("~n =============== ~n ~p : ~p ~n =============== ~n",
		[NameArg1, Arg1]).


-spec maps_get(map_path(), map()) -> Value :: any().
maps_get(Path, Map) ->
	GetFun = maps_getf(Path),
	GetFun(Map).

-spec maps_get(map_path(), map(), any()) -> Value :: any().
maps_get(Path, Map, Default) ->
	GetFun = maps_getf(Path, Default),
	GetFun(Map).


-spec maps_put(map_path(), any(), map()) -> Value :: any().
maps_put(Path, Value, Map) ->
	PutFun = maps_putf(Path),
	PutFun(Value, Map).


-spec maps_remove(map_path(), map()) -> map().
maps_remove(Path, Map) ->
	RemoveFun = maps_removef(Path),
	RemoveFun(Map).


-spec iso_timestamp(timestamp()) -> list().
iso_timestamp(TS) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(TS),
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).


-spec tdiff_seconds(timestamp(), timestamp()) -> non_neg_integer().
tdiff_seconds(T1, T2) ->
	round(abs(timer:now_diff(T1, T2) / 1000000)).


-spec try_ets_lookup(Table :: tid(), Key :: binary()) -> Value :: any().
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).

-spec try_ets_lookup(Table :: tid(), Key :: binary(), Default :: any()) -> Value :: any().
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[{_, Val} | _] -> Val;
		[] -> Default;
		[Object | _] -> Object
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

maps_getf(Path) ->
	fun(Map) ->
		maps_getf_internal(Path, Map) end.

maps_getf_internal([Key | _PathRest], Map) when is_list(Key)->
	lists:foldl(
		fun(Map2, Acc) ->
			Acc ++ [maps_getf_internal(Key, Map2)]
		end
		,[], Map);
maps_getf_internal([Key | PathRest], Map) ->
	try maps:get(Key, Map) of
		Value ->
			maps_getf_internal(PathRest, Value)
	catch
		_:_ ->
			{error, bad_key}
	end;
maps_getf_internal([], Value) ->
	Value.


maps_getf(Path, Default) ->
	fun(Map) ->
		maps_getf_internal(Path, Map, Default) end.

maps_getf_internal([Key | PathRest], Map, Default) ->
	try maps:get(Key, Map, Default) of
		Value ->
			maps_getf_internal(PathRest, Value, Default)
	catch
		_:_ ->
			Default
	end;
maps_getf_internal([], Value, _) ->
	Value.


maps_putf(Path) ->
	fun(Value, Map) ->
		maps_putf_internal(Path, Value, Map) end.

maps_putf_internal([Key | PathRest], Value, Map) ->
	SubMap =
		case maps:is_key(Key, Map) andalso is_map(maps:get(Key, Map)) of
			true ->
				maps:get(Key, Map);
			false ->
				#{}
		end,
	maps:put(Key, maps_putf_internal(PathRest, Value, SubMap), Map);
maps_putf_internal([], Value, _) ->
	Value.


maps_removef(Path) ->
	fun(Map) ->
		maps_removef_internal(Path, Map) end.

maps_removef_internal([], _) ->
	throw({bad_path, []});
maps_removef_internal([LastKey], Map) ->
	maps:remove(LastKey, Map);
maps_removef_internal([Key | PathRest], Map) ->
	case maps:is_key(Key, Map) of
		true ->
			maps:put(Key, maps_removef_internal(PathRest, maps:get(Key, Map)), Map);
		false ->
			Map
	end.
