-module(path).
-author('olivier@biniou.info').

-include("pvc.hrl").

-export([new/1, length/1, clength/1]).


new(Max) ->
    pvc_utils:shuffle(lists:seq(2, Max)).


path(Path0) ->
    [1 | Path0].

length(Path0) ->
    Path = path(Path0),
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
    %% io:format("D= ~p Acc= ~p~n", [D, Acc]),
    %% io:format("List: ~p Acc: ~p, F: ~p~n", [[C2 | Tail], Acc + D, F]),
    sum([C2 | Tail], Acc + D, F).


dist(C1, C2) ->
    [{_Couple, RD, _CD}] = ets:lookup(?DIST, {C1, C2}),
    RD.
cdist(C1, C2) ->
    [{_Couple, _RD, CD}] = ets:lookup(?DIST, {C1, C2}),
    CD.
