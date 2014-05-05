

-module(master_sup_tests).

-include_lib("eunit/include/eunit.hrl").



%%====================================================================
%% Test description
%%====================================================================

master_sup_test_() ->
    {"Test master supervisor", 
     {setup, fun start/0, fun stop/1, fun () ->
					      {inorder, [fun start_link_test/1]}
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
    {StartRes, _} = master_sup:start_link(),
    ?assertEqual(StartRes, ok),
    ?assert(whereis(client_serv_sup) =/= undefined),
    ?assert(whereis(listener_sup) =/= undefined),
    exit(whereis(master_sup), kill).
    


%%====================================================================
%% Helper functions
%%====================================================================
