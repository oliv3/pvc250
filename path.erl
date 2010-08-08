-module(path).
-author('olivier@biniou.info').

-include("tsp.hrl").

-compile([export_all]).

-export([new/1, delete/1]).
-export([get_length/1, get_path/1, display/1, mutate/1]). %% length/1, clength/1]).
%% -export([xover/2]).

-export([init/1, loop/2]).


new(Parent) when is_pid(Parent) ->
    {Path, Length} = ?MODULE:get(Parent),
    spawn(?MODULE, loop, [Path, Length]);
new(Path) when is_list(Path) ->
    L = ?MODULE:length(Path),
    spawn(?MODULE, loop, [Path, L]);
new(Max) ->
    %%io:format("init: ~p~n", [Max]),
    spawn(?MODULE, init, [Max]).

init(Max) ->
    Path = utils:shuffle(lists:seq(2, Max)),
    L = ?MODULE:length(Path),
    loop(Path, L).


delete(Pid) ->
    Pid ! die.


path(Path0) ->
    [1 | Path0].

length(Path0) ->
    Path = path(Path0),
    %%io:format("path(~p)~n", [Path]),
    sum(Path, 0, fun dist/2).
clength(Path0) ->
    Path = path(Path0),
    sum(Path, 0, fun cdist/2).


sum([_Last], Acc, _F) ->
    Acc;
sum([C1, C2 | Tail], Acc, F) ->
    D = if
	    C1 < C2 ->
		F(C1, C2);
	    true ->
		F(C2, C1)
	end,
    %%io:format("D= ~p Acc= ~p~n", [D, Acc]),
    %%io:format("List: ~p Acc: ~p, F: ~p~n", [[C2 | Tail], Acc + D, F]),
    sum([C2 | Tail], Acc + D, F).


dist(C1, C2) ->
    %% io:format("dist ~p ~p~n", [C1, C2]),
    [{_Couple, RD, _CD}] = ets:lookup(?DIST, {C1, C2}),
    RD.
    %% case ets:lookup(?DIST, {C1, C2}) of
    %% 	[{_Couple, RD, _CD}] ->
    %% 	    RD;
	
    %% 	[] ->
    %% 	    io:format("oups no match for dist ~p ~p~n", [C1, C2]),
    %% 	    0
    %% end.
cdist(C1, C2) ->
    [{_Couple, _RD, CD}] = ets:lookup(?DIST, {C1, C2}),
    CD.


get(Pid) ->
    Pid ! {self(), get},
    receive
	{value, P} ->
	    P
    end.

get_length(Pid) ->
    Pid ! {self(), length},
    receive
	{length, L} ->
	    L
    end.

get_path(Pid) ->
    Pid ! {self(), path},
    receive
	{path, L} ->
	    L
    end.

-define(SIZE, 5). %% Show N first and last cities

display(Pid) ->
    {Path, Length} = ?MODULE:get(Pid),
    {Left, Right} = lists:split(?SIZE, Path),
    {RRight, _} = lists:split(?SIZE, lists:reverse(Right)),
    End = lists:reverse(RRight),
    SPath = Left ++ End,
    io:format("~w (~p)~n", [SPath, Length]).


loop(Path, Length) ->
    receive
	{Pid, get} ->
	    Pid ! {value, {Path, Length}},
	    loop(Path, Length);

	{Pid, length} ->
	    Pid ! {length, Length},
	    loop(Path, Length);

	{Pid, path} ->
	    Pid ! {path, Path},
	    loop(Path, Length);

	{mutate, N} when N < 3 ->
	    NewPath = mut_reverse(Path),
	    NewLength = path:length(NewPath),
	    loop(NewPath, NewLength);

	{mutate, N} when N < 6 ->
	    NewPath = mut_short_swap(Path),
	    NewLength = path:length(NewPath),
	    loop(NewPath, NewLength);

	{mutate, N} when N < 9 ->
	    NewPath = mut_long_swap(Path),
	    NewLength = path:length(NewPath),
	    loop(NewPath, NewLength);

	{mutate, N} when N =:= 9 ->
	    NewPath = mut_randomize(Path),
	    NewLength = path:length(NewPath),
	    %% io:format("[d] mut_randomize~nOld= ~p ~p~nNew= ~p ~p~n", [Length, Path, NewLength, NewPath]),
	    loop(NewPath, NewLength);

	{mutate, OtherN} ->
	    %% exit({bad_mutation, OtherN});
	    io:format("[!] unhandled mutation in pid ~p: #~p~n", [self(), OtherN]),
	    loop(Path, Length);

	die ->
	    ok
    end.

%%
%% Mutations
%%
rnd_pos() ->
    crypto:rand_uniform(0, ?NC) + 1.

%%
%% take 2 random -different- integers in [1..?ALPHABET_SIZE]
%%
random2() ->
    Rnd1 = rnd_pos(),
    random2(Rnd1).
random2(Rnd1) ->
    Rnd2 = rnd_pos(),
    if
	Rnd1 =:= Rnd2 ->
	    random2(Rnd1);

	true ->
	    if
		Rnd1 < Rnd2 ->
		    {Rnd1, Rnd2};

		true ->
		    {Rnd2, Rnd1}
	    end
    end.

random2gap() ->
    {Rnd1, Rnd2} = Rnd = random2(),
    if
	Rnd2 - Rnd1 > 1 ->
	    Rnd;
	true ->
	    random2gap()
    end.


mutate(Pid) ->
    Mutation = crypto:rand_uniform(0, ?NB_MUTATIONS),
    Pid ! {mutate, Mutation}.


mut_reverse(C) ->
    C.

mut_short_swap(C) ->
    C.

mut_long_swap(C) ->
    C.

mut_randomize(C) ->
    utils:shuffle(C).

%%
%% Tests
%%

%% Basic chromosome
c() ->
    lists:seq($a, $j).
