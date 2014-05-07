

-module(client_serv_sup_tests).

-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% Actual tests
%%====================================================================

start_link_test() ->
    {StartRes, _} = client_serv_sup:start_link(),
    ?assertEqual(StartRes, ok),
    {StartRes2, _} = client_serv_sup:start_link(),
    ?assertEqual(StartRes2, error).

count_children_test() ->
    ChildCount1 = client_serv_sup:count_children(),
    ?assertEqual(ChildCount1, [{specs,0},
			       {active,0},
			       {supervisors,0},
			       {workers,0}]).


%%====================================================================
%% Helper functions
%%====================================================================
