

-module(listener_sup_tests).

-include_lib("eunit/include/eunit.hrl").



%%====================================================================
%% Test description
%%====================================================================

listener_sup_test_() ->
    {"Test client server supervisor", 
     {setup, fun start/0, fun stop/1, fun () ->
					      {inorder, 
					       [fun start_link_test/1,
						fun count_children_test/1,
						fun start_listener_test/1]}
				      end}}.


%%====================================================================
%% Setup functions
%%====================================================================

start() ->
    ok.

stop(_) ->
    ok.


%%====================================================================
%% Actual tests
%%====================================================================

start_link_test(_) ->
    {StartRes1, _} = listener_sup:start_link(8888, 1),
    ?assertEqual(StartRes1, ok),
    {StartRes2, _} = listener_sup:start_link(8888, 1),
    ?assertEqual(StartRes2, error),
    exit(whereis(listener_sup), kill).

count_children_test(_) ->
    {StartRes1, _} = listener_sup:start_link(8888, 1),
    ?assertEqual(StartRes1, ok),
    ChildCount1 = listener_sup:count_children(),
    ?assertEqual(ChildCount1, [{specs,1},
			       {active,1},
			       {supervisors,0},
			       {workers,1}]),
    exit(whereis(listener_sup), kill).

start_listener_test(_) ->
    {StartRes1, _} = listener_sup:start_link(8888, 1),
    ?assertEqual(StartRes1, ok),
    {Child1, _} = listener_sup:start_listener(),
    ?assertEqual(Child1, ok),
    ChildCount1 = listener_sup:count_children(),
    ?assertEqual(ChildCount1, [{specs,2},
			       {active,2},
			       {supervisors,0},
			       {workers,2}]),
    exit(whereis(listener_sup), kill).


%%====================================================================
%% Helper functions
%%====================================================================
