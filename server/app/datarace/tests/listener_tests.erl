
-module(listener_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/types.hrl").


%%====================================================================
%% Test description
%%====================================================================

request_test_()->
    {"Test listener functionality",
      {setup, 
       fun start/0, 
       fun stop/1, 
       fun (SetupData)->
	       [start_link(SetupData),
		login_false(SetupData),
		login_true(SetupData)]
       end}}.


%%====================================================================
%% Setup functions
%%====================================================================

start() ->
    application:start(crypto),
    application:start(emysql),
    application:start(datarace),
    server:start(),
    {ok, Config} = file:consult(filename:absname("../configs/config")),
    {port, Port} = lists:keyfind(port, 1, Config),
    Port.


stop(_SetupData) ->
    application:stop(datarace),
    application:stop(emysql),
    application:stop(crypto).


%%====================================================================
%% Actual tests
%%====================================================================

start_link(Port) ->
    process_flag(trap_exit, true),
    {SockRes, ListenSocket} = tcp:listen(Port+1),
    {StartRes1, Pid1} = listener:start_link(ListenSocket),
    {StartRes2, Pid2} = listener:start_link(ListenSocket),
    tcp:close(ListenSocket),
    exit(Pid1, shutdown),
    exit(Pid2, shutdown),
    [?_assertEqual(ok, SockRes),
     ?_assertEqual(ok, StartRes1),
     ?_assertEqual(ok, StartRes2)].


login_false(Port) ->
    {Connect, ConnectSocket} = tcp:connect(localhost, Port),
    FalseLoginPacket = tcp:binary_login("sdfaef3434f", "dfsdfgsdfg3434f34f"),
    tcp:send(ConnectSocket, FalseLoginPacket),
    Reply = tcp:recv(ConnectSocket),
    [?_assertEqual(ok, Connect),
     ?_assertEqual(ok, Reply)].


login_true(Port) ->
    {Connect, ConnectSocket} = tcp:connect(localhost, Port),
    FalseLoginPacket = tcp:binary_login("test1", "test1"),
    tcp:send(ConnectSocket, FalseLoginPacket),
    Reply = tcp:recv(ConnectSocket),
    [?_assertEqual(ok, Connect),
     ?_assertEqual({tcp, ?LOGIN_TRUE}, Reply)].



%%====================================================================
%% Helper functions
%%====================================================================


%% fail_login() ->
%%     Packet = tcp:binary_login("rubbishusername", "rubbishpassword"),
%%     gen_tcp:send(CPacket),
%%     tcp:recv(ConnectSocket).
