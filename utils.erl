-module(utils).
-author('olivier@biniou.info').

-export([shuffle/1]).


%% -- shuffle/1
%%
%% Based on http://www.trapexit.org/RandomShuffle
%% Refactored through tidier
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
			randomize(Acc)
		end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = [{random:uniform(), A} || A <- List],
    {_, D1} = lists:unzip(lists:keysort(1, D)), 
    D1.
