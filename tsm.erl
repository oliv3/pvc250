-module(tsm).
-author('olivier@biniou.info').

-export([start/0, start/1]).


start() ->
    start("test.csv").

start(File) ->
    N = cities:start(File),
    ok.
