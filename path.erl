-module(path).
-author('olivier@biniou.info').

-include("tsp.hrl").

%% TODO: dans la loop, stocker le chromosome en binary

%%-ifdef(DEBUG).
-compile([export_all]).
%%-endif.

-export([new/1, delete/1]).
-export([get_length/1, get_path/1, display/1, mutate/1]).
-export([rnd_pos/0, random2/0]).

-export([init/1, loop/2]).


new(Parent) when is_pid(Parent) ->
    {Path, Length} = ?MODULE:get(Parent),
    spawn(fun () -> (?MODULE):loop(Path, Length) end);
new(Path) when is_list(Path) ->
    L = ?MODULE:length(Path),
    spawn(fun () -> (?MODULE):loop(Path, L) end);
new(Max) ->
    %% io:format("init: ~p~n", [Max]),
    spawn(fun () -> (?MODULE):init(Max) end).

init(Max) ->
    Path = utils:shuffle(lists:seq(2, Max)),
    Length = ?MODULE:length(Path),
    loop(Path, Length).


delete(Pid) ->
    Pid ! die.


path(Path0) ->
    [1 | Path0].

length(Path0) ->
    Path = path(Path0),
    %% io:format("path(~p)~n", [Path]),
    sum(Path, 0).


sum([_Last], Acc) ->
    Acc;
sum([C1, C2 | Tail], Acc) ->
    D = if
	    C1 < C2 ->
		dist(C1, C2);
	    true ->
		dist(C2, C1)
	end,
    %% io:format("D= ~p Acc= ~p~n", [D, Acc]),
    %% io:format("List: ~p Acc: ~p, F: ~p~n", [[C2 | Tail], Acc + D, F]),
    sum([C2 | Tail], Acc + D).


dist(C1, C2) ->
    %% io:format("dist ~p ~p~n", [C1, C2]),
    [{_Couple, RD}] = ets:lookup(?DIST, {C1, C2}),
    RD.


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
rnd_pos_m1() ->
    crypto:rand_uniform(0, ?NC-1) + 1.

%%
%% take 2 random -different- integers in [1..NumberOfCities]
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


-ifdef(DEBUG).
-define(D_F(F, A), io:format(F, A)).
-define(D_MUTATE(C, X), io:format(" 1234567890~n ----------~n ~s~n ~s~n ----------~n", [C, X])).
-else.
-define(D_F(F, A), nop).
-define(D_MUTATE(C, X), nop).
-endif.

%%
%% Reverse a chromosome (completely or partially)
%%
mut_reverse(C) ->
    Positions = random2gap(),
    mut_reverse(C, Positions).
mut_reverse(C, {P1, P2} = _Pos) when P1 =:= 1 andalso P2 =:= ?NC ->
    NewC = lists:reverse(C),
    ?D_F("mut_reverse/2 pos: ~p (full)~n~n", [_Pos]),
    ?D_MUTATE(C, NewC),
    NewC;
mut_reverse(C, {P1, P2} = _Pos) ->
    {Rest, Right} = lists:split(P2, C),
    {Left, Middle} = lists:split(P1-1, Rest), %% FIXME return always -1 ?
    RMiddle = lists:reverse(Middle),
    ?D_F("mut_reverse/2 pos: ~p (partial)~n~n", [_Pos]),
    NewC = Left ++ RMiddle ++ Right,
    ?D_MUTATE(C, NewC),
    NewC.


%%
%% Swap two adjacent genes in a chromosome
%%
mut_short_swap(C) ->
    Pos1 = rnd_pos_m1(),
    mut_short_swap(C, Pos1).

mut_short_swap(C, Pos1) ->
    Pos2 = Pos1+1,

    %% Tuple = list_to_tuple(C),
    %% E1 = element(Pos1, Tuple),
    %% E2 = element(Pos2, Tuple),

    %% Tmp1 = setelement(Pos1, Tuple, E2),
    %% Tmp2 = setelement(Pos2, Tmp1, E1),
    %% NewC = tuple_to_list(Tmp2),

    NewC = swap(C, {Pos1, Pos2}),
    ?D_F("mut_short_swap/2 pos= ~p~n~n", [Pos1]),
    ?D_MUTATE(C, NewC),
    NewC.

%%
%% Swap two distant genes in a chromosome
%%
mut_long_swap(C) ->
    Pos = random2gap(),
    mut_long_swap(C, Pos).

mut_long_swap(C, _Pos) ->
    NewC = swap(C, _Pos),
    ?D_F("mut_long_swap/2 pos= ~p~n~n", [_Pos]),
    ?D_MUTATE(C, NewC),
    NewC.

swap(C, {Pos1, Pos2}) ->
    Tuple = list_to_tuple(C),
    E1 = element(Pos1, Tuple),
    E2 = element(Pos2, Tuple),

    Tmp1 = setelement(Pos1, Tuple, E2),
    Tmp2 = setelement(Pos2, Tmp1, E1),
    tuple_to_list(Tmp2).


%%
%% Randomize a chromosome
%%
mut_randomize(C) ->
    utils:shuffle(C).

%%
%% Tests
%%

%% Basic chromosome
c() ->
    lists:seq($a, $j).


-ifdef(DEBUG).
test_mut_reverse_full() ->
    C = c(),
    mut_reverse(C, {1, ?NC}).

test_mut_reverse_partial() ->
    C = c(),
    mut_reverse(C).

test_mut_short_swap() ->
    C = c(),
    mut_short_swap(C).

test_mut_long_swap() ->
    C = c(),
    mut_long_swap(C).

-endif.
