-module(tsp).
-author('olivier@biniou.info').

-include("tsp.hrl").

-compile([export_all]).

-export([start/0, start/1]).

%% Best solution so far:
%% d= 11.8093

%% GA parameters
-define(POP_SIZE, 50000).

%% Cross-over et mutations
-define(P_XOVER,    80). %% 80%
-define(P_MUTATION, 3).  %% une chance sur 3

%% CPU cooling pauses
-define(TOS, 5). %% seconds
-define(TOM, ?TOS*1000).

-define(TOP, 10).
-define(H_POP_SIZE, (?POP_SIZE bsr 1)).

-record(state, {n, pids}).


start() ->
    %% start("test.csv").
    start("defi250.csv").

start(File) ->
    crypto:start(),
    N = cities:start(File),
    Pids = new_population(?POP_SIZE, N),
    %% io:format("Population:~n~p~n", [Pids]),
    %% register(?TSM, self()),
    loop(#state{n=N, pids=Pids}).


new_population(Size, N) ->
    [path:new(N) || _C <- lists:seq(1, Size)].


receive_eval(Ref) ->
    receive
	{Ref, length, Result} ->
	    Result
    end.


loop(#state{n=N, pids=Pids} = State) ->
    %% io:format("[d] Pids:~n~p~n", [Pids]),

    Evals1 = [{Pid, path:get_length(Pid)} || Pid <- Pids],
    %% io:format("[d] Evals1:~n~p~n", [Evals1]),
    
    TotalScore = lists:sum([Score || {_Path, Score} <- Evals1]),
    %% io:format("[d] Total score: ~p~n", [TotalScore]),

    Evals2 = [{Path, TotalScore-Score} || {Path, Score} <- Evals1],
    %% io:format("[d] Evals2:~n~p~n", [Evals2]),

    Evals3 = lists:reverse(lists:keysort(2, Evals2)),
    %% io:format("[d] Evals3:~n~p~n", [Evals3]),

    %% Total score for the roulette
    TotalScore2 = lists:sum([Score || {_Path, Score} <- Evals3]),
    %% io:format("[d] Total score 2: ~p~n", [TotalScore2]),

    %% Display Top N
    Top = top(Evals3),
    %% io:format("~n[*] Generation: ~p, ~p individuals evaluated~n", [Gen, Gen*?POP_SIZE]),
    io:format("[i] ~p processes~n~n", [length(processes())]),
    io:format("[*] Top ~p:~n", [?TOP]),
    [path:display(Pid) || {Pid, _Score} <- Top],
    io:format("~n", []),

    %% Create new population
    Refs = new_population(N, ?H_POP_SIZE, Evals3, TotalScore2),
    ChildrenPids = receive_refs(Refs),
    %% io:format("[d] ChildrenPids: ~p~n", [ChildrenPids]),
    NewPids = lists:flatten(ChildrenPids),
    %% io:format("[d] NewPids: ~p~n", [NewPids]),

    %% Kill old population
    [path:delete(Pid) || Pid <- Pids],

    timer:sleep(1000),

    %% Next generation
    loop(State#state{pids=NewPids}).
    

top(List) ->
    {L1, _} = lists:split(?TOP, List),
    L1.


%%
%% Create a new population
new_population(NCities, N, Population, MaxScore) ->
    new_population(NCities, N, Population, MaxScore, []).
new_population(_NCities, 0, _Population, _MaxScore, Acc) ->
    Acc;
new_population(NCities, N, Population, MaxScore, Acc) ->
    Parent1 = roulette(Population, MaxScore, undefined),
    Parent2 = roulette(Population, MaxScore, Parent1),
    Ref = make_ref(),
    V = self(),
    %% io:format("spawning MATE: ~p ~p~n", [Parent1, Parent2]),
    spawn(fun() -> ?MODULE:mate(V, Ref, NCities, Parent1, Parent2) end),
    new_population(NCities, N-1, Population, MaxScore, [Ref | Acc]).


mate(Pid, Ref, NCities, Pid1, Pid2) ->
    P = crypto:rand_uniform(0, 101),
    %% io:format("*** Rand: ~p~n", [P]),
    if
	P =< ?P_XOVER ->
	    xover(Pid, Ref, NCities, Pid1, Pid2);

	true ->
	    %% io:format("MATE: new/1 ~p~n", [Parent1]),
	    Child1 = path:new(Pid1),
	    %% io:format("MATE: new/2 ~p~n", [Parent2]),
	    Child2 = path:new(Pid2),
	    maybe_mutate(Child1),
	    maybe_mutate(Child2),
	    %% io:format("RESULT 1: ~p ~p~n", [Child1, Child2]),
	    Pid ! {Ref, [Child1, Child2]}
    end.

    
roulette(Population, MaxScore, NotThisPid) ->
    <<B>> = crypto:rand_bytes(1),
    FB = B / 255,
    %% Score = crypto:rand_uniform(0, MaxScore),
    Score = FB * MaxScore,
    Pid = extract(Population, Score),
    if
	Pid =:= NotThisPid ->
	    roulette(Population, MaxScore, NotThisPid);

	true ->
	    Pid
    end.


extract(Population, Score) ->
    extract(Population, Score, 0).
extract([{Pid, S} | Population], Score, CurScore) ->
    NewScore = CurScore + S,
    if
	NewScore >= Score ->
	    %% io:format("[d] Found element ~p, score ~p -> ~p >= ~p~n~n", [f(Element), S, NewScore, Score]),
	    Pid;
	true ->
	    %% io:format("[d] Skip element ~p, score ~p -> ~p  < ~p~n",   [f(Element), S, NewScore, Score]),
	    extract(Population, Score, NewScore)
    end.


receive_refs(Refs) ->
    %% io:format("Waiting for ~p refs (~p)~n", [length(Refs), Refs]),
    receive_refs(Refs, []).
receive_refs([], Pids) ->
    Pids;
receive_refs([Ref|Refs], Acc) ->
    receive
	{Ref, Pids} ->
	    receive_refs(Refs, [Pids | Acc])
    end.


maybe_mutate(Pid) ->
    case crypto:rand_uniform(0, ?P_MUTATION) of
	0 ->
	    path:mutate(Pid);
	_Other ->
	    ok
    end.


xover(Pid, Ref, NCities, Parent1, Parent2) ->
    %% {_, _, MS} = now(),
    Chrom1 = path:get_path(Parent1),
    Chrom2 = path:get_path(Parent2),
    Parents = {Chrom1, Chrom2},
    %% io:format("xover: Parents= ~p~n", [Parents]),
    Cut = crypto:rand_uniform(1, NCities),
    {Child1, Child2} = xover1(Cut, Parents),
    %% {Child1, Child2} = case MS rem 2 of
    %% 			   0 ->
    %% 			       xover1(Parents);
    %% 			   1 ->
    %% 			       xover2(Parents)
    %% 		       end,

    Pid1 = path:new(Child1),
    Pid2 = path:new(Child2),
    maybe_mutate(Pid1),
    maybe_mutate(Pid2),
    %%io:format("RESULT X: ~p ~p~n", [Pid1, Pid2]),
    Pid ! {Ref, [Pid1, Pid2]}.


xover1(Cut, {Path1, Path2}) ->
    {L1, R1} = lists:split(Cut, Path1),
    {L2, R2} = lists:split(Cut, Path2),
    
    NR1 = [[L2--L1]|[R2--L1]],
    NR2 = [[L1--L2]|[R1--L2]],

    C1 = [L1|NR1],
    C2 = [L2|NR2],

    {lists:flatten(C1), lists:flatten(C2)}.

%% xover2: on fait 2x xover1 avec 2 cutpoints distincts

test() ->
    P1 = lists:seq(1, 9),
    P2 = [4,1,6,3,9,8,2,5,7],
    Rnd = 4,
    xover1(Rnd, {P1, P2}).