
-module(listener_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/types.hrl").


%%====================================================================
%% Actual tests
%%====================================================================

start_link_test() ->
    process_flag(trap_exit, true),
    {SockRes, ListenSocket} = tcp:listen(8888),
    ?assertEqual(ok, SockRes),
    {StartRes1, Pid1} = listener:start_link(ListenSocket),
    {StartRes2, Pid2} = listener:start_link(ListenSocket),
    ?assertEqual(ok, StartRes1),
    ?assertEqual(ok, StartRes2),
    tcp:close(ListenSocket),
    exit(Pid1, shutdown),
    exit(Pid2, shutdown).

login_false_test() ->
    process_flag(trap_exit, true),
    {MasterStartRes, Pid} = master_sup:start_link(),
    ?assertEqual(ok, MasterStartRes),
    Message = spawn_fail_login(),
    ?assertEqual({tcp, ?LOGIN_FALSE_USERNAME}, Message),
    exit(Pid, shutdown).		       


%%====================================================================
%% Helper functions
%%====================================================================


spawn_fail_login() ->
    {ok, ConnectSocket} = tcp:connect(localhost, 8888),
    Packet = tcp:binary_login("rubbishusername", "rubbishpassword"),
    tcp:send(ConnectSocket, Packet),
    tcp:recv(ConnectSocket).
