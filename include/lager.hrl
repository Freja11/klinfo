%%===================%%
%%		LAGER
%%===================%%


-define(ERROR(M, Arg), lager:error(M, Arg)).
-define(WARN(M, Arg), lager:warning(M, Arg)).
-define(INFO(M, Arg), lager:info(M, Arg)).

-define(LOGGER_OUTPUT_MAX_LENGTH, 100).
