

-module(listener_sup_tests).

-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% Actual tests
%%====================================================================

start_link_and_listener_test() ->
    {StartRes1, Pid} = listener_sup:start_link(8888, 2),
    ?assertEqual(ok, StartRes1),
    {StartRes2, _} = listener_sup:start_link(8888, 1),
    ?assertEqual(error, StartRes2),
    {ChildRes1, _} = listener_sup:start_listener(),
    ?assertEqual(ok, ChildRes1),
    timer:sleep(100).

count_children_test() ->
    process_flag(trap_exit, true),
    ChildCount1 = listener_sup:count_children(),
    ?assertEqual([{specs,1},
		  {active,3},
		  {supervisors,0},
		  {workers,3}],
		 ChildCount1),
    exit(whereis(listener_sup), shutdown).


%%====================================================================
%% Helper functions
%%====================================================================