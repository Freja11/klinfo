%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%% SYSTEM SETTINGS
-define(NODE, node()).
-define(APPLICATION, klinfo).
-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).
-define(DEFAULT_PORT, 13028). %% todo check free available ports
-define(POOLER_MAX_PROCESSES, 20).
-define(POOLER_MIN_PROCESSES, 10).

-define(TABLES, [
	<<"user">>,
	<<"task">>,
	<<"project">>,
	<<"geo">>,
	<<"mileage">>
]).



%%%%%%%%%%%%%%%%%%%%%%
%%%    TYPES	   %%%
%%%%%%%%%%%%%%%%%%%%%%


-type table() :: user | task | project.
-type crud_verb() :: insert | read | read_all | update | update_part | delete | delete_all.
-type message() :: {table(), tuple()}.
-export_type([crud_verb/0, table/0, message/0]).
