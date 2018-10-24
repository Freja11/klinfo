-module(klinfo_endpoint).

-include("lager.hrl").

%% API
-export([init/2]).
-export([content_types_accepted/2, allowed_methods/2]).
-export([handle_post/2]).


%%%%%%%%%%%%%%%%%%%%%%
%%%   COWBOY API   %%%
%%%%%%%%%%%%%%%%%%%%%%

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


content_types_accepted(Req, State) ->
	{[
		{<<"application/json">>, handle_post}
	], Req, State}.


allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%
%%%  HANDLE METHODS  %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Handle
%%
handle_post(Req, State) ->
	%% {ok, BodyJson, _} = cowboy_req:read_body(Req),
	%% BR = jsx:decode(BodyJson, [return_maps]),
	%% Reply = parser_worker:parse_br(BR),
	{true, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
