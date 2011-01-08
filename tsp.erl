-module(tsp).
-author('olivier@biniou.info').

-include("tsp.hrl").

-compile([export_all]).

-export([start/0, start/1]).

%% TODO: profiling (fprof/eprof)

%% Best solution so far:
%% d= 11.8093

%% GA parameters
%% 40000 seems to be an upper limit for now, setting to 35K to be safe --oliv3
-define(POP_SIZE, 35000).
%% -define(POP_SIZE, 100). %% testing

%% Cross-over et mutations
-define(P_XOVER,    65). %% 65%
-define(P_MUTATION, 10). %% une chance sur 10

%% CPU cooling pauses
-define(TOS, 5). %% seconds
-define(TOM, ?TOS*1000).

-define(TOP, 10).
-define(H_POP_SIZE, (?POP_SIZE bsr 1)).

-record(state, {n, pids, gen=1}).

-ifdef(DEBUG).
start() ->
    start("test.csv").
-else.
start() ->
    start("defi250.csv").
-endif.


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


max(List) ->
    ?MODULE:max(List, -1).
max([], Max) ->
    Max;
max([H|Tail], Max) ->
    NewMax = erlang:max(H, Max),
    ?MODULE:max(Tail, NewMax).

hr() ->
    io:format("~s~n", [lists:duplicate(79, $=)]).


loop(#state{n=N, pids=Pids, gen=Gen} = State) ->
    %% io:format("[d] Pids:~n~p~n", [Pids]),

    Evals1 = [{Pid, path:get_length(Pid)} || Pid <- Pids],
    %% io:format("[d] Evals1:~n~p~n", [Evals1]),
    
    %% TotalScore = lists:sum([Score || {_Path, Score} <- Evals1]),
    %% io:format("[d] Total score: ~p~n", [TotalScore]),

    MaxLength = max([Score || {_Path, Score} <- Evals1]),
    hr(),

    %% Evals2 = [{Path, TotalScore-Score} || {Path, Score} <- Evals1],
    Evals2 = [{Path, MaxLength-Length} || {Path, Length} <- Evals1],
    %% io:format("[d] Evals2:~n~p~n", [Evals2]),

    Evals3 = lists:reverse(lists:keysort(2, Evals2)),
    %% io:format("[d] Evals3:~n~p~n", [Evals3]),

    %% Total score for the roulette
    TotalScore2 = lists:sum([Score || {_Path, Score} <- Evals3]),
    %% io:format("[d] Total score 2: ~p~n", [TotalScore2]),

    %% Display Top N
    Top = top(Evals3),
    io:format("[*] Generation: ~p, ~p individuals evaluated~n", [Gen, Gen*?POP_SIZE]),
    io:format("[d] Max length: ~p~n", [MaxLength]),
    %% io:format("[i] ~p processes~n", [length(processes())]),
    hr(),
    io:format("[*] Top ~p:~n", [?TOP]),
    %% io:format("[*] Top ~p: ~p~n", [?TOP, Top]),
    [path:display(Pid) || {Pid, _Score} <- Top],
    hr(),
    io:format("~n", []),

    %% Create new population
    Refs = new_population(N, ?POP_SIZE, Evals3, TotalScore2),
    ChildrenPids = receive_refs(Refs),
    %% io:format("[d] ChildrenPids: ~p~n", [ChildrenPids]),
    NewPids = lists:flatten(ChildrenPids),
    %% io:format("[d] NewPids: ~p~n", [NewPids]),

    %% Kill old population
    [path:delete(Pid) || Pid <- Pids],

    timer:sleep(1000),

    %% Next generation
    ?MODULE:loop(State#state{pids=NewPids, gen=Gen+1}).
    

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
    spawn(fun() -> ?MODULE:mate(V, Ref, Parent1, Parent2) end),
    new_population(NCities, N-1, Population, MaxScore, [Ref | Acc]).


mate(Pid, Ref, Pid1, Pid2) ->
    P = crypto:rand_uniform(0, 101),
    %% io:format("*** Rand: ~p~n", [P]),
    if
	P =< ?P_XOVER ->
	    xover(Pid, Ref, Pid1, Pid2);

	true ->
	    %% io:format("MATE: new/1 ~p~n", [Parent1]),
	    Child1 = path:new(Pid1),
	    %% io:format("MATE: new/2 ~p~n", [Parent2]),
	    %% Child2 = path:new(Pid2),
	    maybe_mutate(Child1),
	    %% maybe_mutate(Child2),
	    %% io:format("RESULT 1: ~p ~p~n", [Child1, Child2]),
	    Pid ! {Ref, Child1}
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


xover(Pid, Ref, Parent1, Parent2) ->
    {_, _, MS} = now(),
    Chrom1 = path:get_path(Parent1),
    Chrom2 = path:get_path(Parent2),
    Parents = {Chrom1, Chrom2},
    %% io:format("xover: Parents= ~p~n", [Parents]),

    %% Cut = crypto:rand_uniform(1, NCities),
    %% {Child1, Child2} = xover1(Cut, Parents),

    Child = case MS rem 2 of
		%% _ -> %% 0 ->
		0 ->
		    %% io:format("1"),
		    xover1(Parents);
		1 ->
		    %% io:format("2"),
		    xover2(Parents)
	    end,

    ChildPid = path:new(Child),
    maybe_mutate(ChildPid),
    Pid ! {Ref, ChildPid}.


xover1(Parents) ->
    Cut = path:rnd_pos(),
    xover1(Cut, Parents).
xover1(Cut, {Path1, Path2}) ->
    {L1, _R1} = lists:split(Cut, Path1),
    {L2, R2} = lists:split(Cut, Path2),
    
    NR1 = [[L2--L1]|[R2--L1]],
    %% NR2 = [[L1--L2]|[R1--L2]],

    C1 = [L1|NR1],
    %% C2 = [L2|NR2],

    lists:flatten(C1).


xover2(Parents) ->
    Pos = path:random2(),
    xover2(Pos, Parents).
xover2({Cut1, Cut2}, {Path1, _Path2} = Parents) ->
    Child = xover1(Cut1, Parents),
    xover1(Cut2, {Child, Path1}).


test_parents() ->
    P1 = lists:seq(1, 9),
    P2 = [4,1,6,3,9,8,2,5,7],
    {P1, P2}.

test_xover1() ->
    Parents = test_parents(),
    Rnd = 4,
    xover1(Rnd, Parents).

test_xover2() ->
    Parents = test_parents(),
    Rnd = {2, 5},
    xover2(Rnd, Parents).


%% xover2 check:

%% Parents:
%% {[1,2,3,4,5,6,7,8,9],[4,1,6,3,9,8,2,5,7]}
%% Children:
%% > tsp:test_xover2().
%% {[1,2,4,6,3,5,7,8,9],[4,1,2,3,5,6,9,8,7]}

%% Step1:
%% [1,2|3,4,5,6,7,8,9]
%% [4,1|6,3,9,8,2,5,7]
%% Result1:
%% [1,2,4,6,3,9,8,5,7]
%% [4,1,2,3,5,6,7,8,9]

%% Step2:
%% [1,2,4,6,3|9,8,5,7]
%% [4,1,2,3,5|6,7,8,9]
%% Result2:
%% [1,2,4,6,3,5,7,8,9]
%% [4,1,2,3,5,6,7,8,9]

%% Which seems to be ok \o/
