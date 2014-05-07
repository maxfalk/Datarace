

-module(master_sup_tests).

-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% Actual tests
%%====================================================================

start_link_test() ->
    process_flag(trap_exit, true),
    {StartRes, Pid} = master_sup:start_link(),
    ?assertEqual(ok, StartRes),
    ?assert(whereis(client_serv_sup) =/= undefined),
    ?assert(whereis(listener_sup) =/= undefined),
    exit(whereis(master_sup), shutdown).


%%====================================================================
%% Helper functions
%%====================================================================
