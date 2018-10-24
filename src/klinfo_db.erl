-module(klinfo_db).

%% API
-export([user_keys/0,
	task_keys/0,
	project_keys/0,
	geo_keys/0,
	mileage_keys/0
]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

user_keys() ->
	[
		<<"id">>,
		<<"firstname">>,
		<<"lastname">>,
		<<"email">>,
		<<"password">>,
		<<"gender">>,
		<<"dob">>,
		<<"age">>,
		<<"phone_number">>,
		<<"address">>,
		<<"role">>,
		<<"hire_date">>,
		<<"position">>,
		<<"timestamp">>
	].


task_keys() ->
	[
		<<"id">>,
		<<"title">>,
		<<"description">>,
		<<"timestamp">>,
		<<"start_datetime">>,
		<<"end_datetime">>,
		<<"project">>,
		<<"supervisor">>,
		<<"employees">>
	].


project_keys() ->
	[
		<<"id">>,
		<<"title">>,
		<<"description">>,
		<<"timestamp">>,
		<<"start_datetime">>,
		<<"end_datetime">>,
		<<"tasks">>,
		<<"tasks">>,
		<<"supervisor">>,
		<<"employees">>
	].

geo_keys() ->
	[

	].


mileage_keys() ->
	[
		<<"id">>,
		<<"title">>,
		<<"descritpion">>,
		<<"employees">>,
		<<"origin">>,
		<<"destination">>,
		<<"timestamp">>,
		<<"start_datetime">>,
		<<"end_datetime">>
	].


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
