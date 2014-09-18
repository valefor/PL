-module(qlc_test).
-compile({no_auto_import,[error/1]}).
-export([test/0,q/1,q/2]).
-record(qlc_lc,     % qlc:q/1,2
        {lc,
         opt        % #qlc_opt
        }).

-record(qlc_handle, {h}).

-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).

test() ->
    %do(?MODULE:q([X || X <- mnesia:table(shop)])),
    do(?MODULE:q([X || X <- tt:get_table()])).

q(QLC_lc) ->
    q(QLC_lc, []).

q(#qlc_lc{}=QLC_lc, Options) ->
    #qlc_handle{h = QLC_lc#qlc_lc{opt = Options}};
q(T1, T2) ->
    io:format("T1:~p|T2~p~n",[T1, T2]),
    erlang:error(badarg, [T1, T2]).

do(Q) ->
    io:format("Q:~p~n",[Q]).

