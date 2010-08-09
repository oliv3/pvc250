%% ETS table
-define(DIST, dist).

%% GA mutations
-define(NB_MUTATIONS, 10).

-define(DEBUG, true).

%% XXX hardocoded for efficiency reasons

%% Number of Real Cities
-ifdef(DEBUG).
-define(NRC, 11).
-else.
-define(NRC, 250).
-endif.

-define(NC, (?NRC-1)). %% Number of Cities in a chromosome
