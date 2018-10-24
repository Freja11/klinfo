-module(klinfo_api).

-include("global.hrl").

%% API
-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2, allowed_methods/2]).
-export([handle_post_or_put/2, handle_get/2, delete_resource/2]).


%%%%%%%%%%%%%%%%%%%%%%
%%%   COWBOY API   %%%
%%%%%%%%%%%%%%%%%%%%%%

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, handle_get}
	], Req, State}.


content_types_accepted(Req, State) ->
	{[
		{<<"application/json">>, handle_post_or_put}
	], Req, State}.


allowed_methods(Req, State) ->
	{[<<"POST">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%
%%%  HANDLE METHODS  %%%
%%%%%%%%%%%%%%%%%%%%%%%%

handle_post_or_put(Req, State) ->
	{ok, BodyJson, _} = cowboy_req:read_body(Req),
	BodyMap = jsx:decode(BodyJson, [return_maps]),
	klinfo_utils:echo1("**** MAP: ", BodyMap),
	Result = case cowboy_req:binding(id, Req) of
				 undefined ->
					 Table = get_table(post, Req),
					 klinfo_utils:echo1("**** Table: ", Table),
					 internal_post(Table, BodyMap);
				 Id ->
					 klinfo_utils:echo1("**** Id: ", Id),
					 Table = get_table(put, Req),
					 klinfo_utils:echo1("**** Table: ", Table),
					 internal_put(Table, Id, BodyMap)
			 end,
	{Result, Req, State}.


handle_get(Req, State) ->
	Id = cowboy_req:binding(id, Req),
	klinfo_utils:echo1("**** ID: ", Id),
	Result = case klinfo_worker:crud(read_all, {user, Id}) of
				 {ok, Data} ->
					 jsx:encode(Data);
				 _ ->
					 false
			 end,
	{Result, Req, State}.


delete_resource(Req, State) ->
	Value = cowboy_req:binding(id, Req),
%%	Result = case dbc:delete_student(Value) of
%%				 {ok, _} ->
%%					 true;
%%				 _ ->
%%					 false
%%			 end,
	{result, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

internal_post(Table, BodyMap) ->
	case klinfo_worker:crud(insert, {Table, BodyMap}) of
		{ok, _} -> true;
		_ -> false
	end.


internal_put(Table, Id, BodyMap) ->
	case klinfo_worker:crud(update, {Table, {Id, BodyMap}}) of
		{ok, _} -> true;
		_ -> false
	end.


get_table(post, Req) ->
	Path = cowboy_req:path(Req),
	get_table_atom_post(Path);
get_table(_, Req) ->
	Path = cowboy_req:path(Req),
	get_table_atom(Path).


get_table_atom_post(<<"/user">>) -> user;
get_table_atom_post(<<"/task">>) -> task;
get_table_atom_post(<<"/project">>) -> project;
get_table_atom_post(<<"/geo">>) -> geo;
get_table_atom_post(<<"/mileage">>) -> mileage;
get_table_atom_post(_) -> invalid_path.

get_table_atom(Path) ->
	check_path(?TABLES, Path).


check_path([], _) -> invalid_path;
check_path([CurrentTable | Tail], Path) ->
	case binary:match(Path, CurrentTable) of
		nomatch ->
			check_path(Tail, Path);
		_Pattern ->
			table_to_binary(CurrentTable)
	end.


table_to_binary(<<"user">>) -> user;
table_to_binary(<<"task">>) -> task;
table_to_binary(<<"project">>) -> project;
table_to_binary(<<"geo">>) -> geo;
table_to_binary(<<"mileage">>) -> mileage;
table_to_binary(_) -> invalid_path.
