-module(cities).
-author('olivier@biniou.info').

-include("tsm.hrl").

-export([start/1, stop/0]).
-export([init/2]).

-define(SERVER, ?MODULE).

%% ETS tables
%%
%% Cities: {N, {X, Y} = RealPos, {Xc, Yc} = CorrectedPos}
%% Dist: {{N1, N2}, RealDist, CorrectedDist} %% named_table, N1 < N2
%%

start(File) ->
    {ok, Bin} = file:read_file(File),
    L0 = binary_to_list(Bin),
    [N0|Cities] = string:tokens(L0, [10]),
    N1 = list_to_integer(N0),
    io:format("[+] Loading ~p cities~n", [N1]),
    SPid = spawn(?MODULE, init, [N1, Cities]),
    register(?SERVER, SPid),
    N1.

init(N, Cities) ->
    Tid1 = ets:new(cities, []),
    load_cities(Tid1, Cities),
    ets:new(?DIST, [named_table]),
    init_dist(N, Tid1),
    ets:delete(Tid1),
    loop().


stop() ->
    ?SERVER ! stop.


load_cities(Tid, List) ->
    load_cities(Tid, List, 1).

load_cities(_Tid, [], _N) ->
    ok;
load_cities(Tid, [City|Cities], N) ->
    [LX, LY] = string:tokens(City, [$;]),
    X = list_to_float(LX),
    Y = list_to_float(LY),
    T = {N, {X, Y}, {X, Y, X*Y}}, %% XXX X*Y a revoir
    ets:insert(Tid, T),
    load_cities(Tid, Cities, N+1).


init_dist(N, Tid) ->
    F = fun() -> lists:seq(1, N) end,
    D = [{X, Y} || X <- F(), Y <- F()],
    [insert_dist(C, Tid) || C <- D].

insert_dist({C1, C2} = Key, Tid) when C1 < C2 ->
    [P1] = ets:lookup(Tid, C1),
    [P2] = ets:lookup(Tid, C2),
    Dist = dist(P1, P2),
    CDist = cdist(P1, P2),
    ets:insert(?DIST, {Key, Dist, CDist});
insert_dist(_Couple, _Tid) ->
    ok.

dist({_N1, {X1, Y1}, _C1}, {_N2, {X2, Y2}, _C2}) ->
    DX = X1 - X2,
    DY = Y1 - Y2,
    DX2 = DX * DX,
    DY2 = DY * DY,
    math:sqrt(DX2 + DY2).
cdist({_N1, _R1, {X1, Y1, _Z1}}, {_N2, _R2, {X2, Y2, _Z2}}) ->
    DX = X1 - X2,
    DY = Y1 - Y2,
    %% DZ = Z1 - Z2,
    DX2 = DX * DX,
    DY2 = DY * DY,
    %% DZ2 = DZ * DZ,
    %% when ready for 3D
    %% math:sqrt(DX2 + DY2 + DZ2).
    math:sqrt(DX2 + DY2).


loop() ->
    receive
	stop ->
	    ok
    end.
