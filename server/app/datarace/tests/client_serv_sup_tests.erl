

-module(client_serv_sup_tests).

-include_lib("eunit/include/eunit.hrl").



%%====================================================================
%% Test description
%%====================================================================

client_serv_sup_test_() ->
    {"Test client server supervisor", 
     {setup, fun start/0, fun stop/1, fun () ->
					      {inorder, 
					       [fun start_link_test/1,
						fun count_children_test/1,
						fun start_client_serv_test/1]}
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
    {StartRes, _} = client_serv_sup:start_link(),
    ?assertEqual(StartRes, ok),
    {StartRes2, _} = client_serv_sup:start_link(),
    ?assertEqual(StartRes2, error).

count_children_test(_) ->
    ChildCount1 = client_serv_sup:count_children(),
    ?assertEqual(ChildCount1, [{specs,0},
			       {active,0},
			       {supervisors,0},
			       {workers,0}]).

start_client_serv_test(_) ->
    {Child1, _} = client_serv_sup:start_client_serv(1, fake_socket),
    ?assertEqual(Child1, ok).


%%====================================================================
%% Helper functions
%%====================================================================
