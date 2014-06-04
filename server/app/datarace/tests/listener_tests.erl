
-module(listener_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/types.hrl").

-define(TIMEOUT_VALUE, 3000).
-define(SLEEP_VALUE, 100).


%%====================================================================
%% Test description
%%====================================================================

listener_test_()->
    {"Test listener functionality: connect, login, register",
      {setup, 
       fun start/0, 
       fun stop/1, 
       fun (SetupData) ->
	       [listener_connect(SetupData),
		listener_register(SetupData),
		listener_login_true(SetupData),
		listener_login_false_logged_in(SetupData),
		listener_login_false_password(SetupData),
		listener_login_false_username(SetupData)]
       end}}.


%%====================================================================
%% Setup functions
%%====================================================================

start() ->
    {ok, Config} = file:consult(filename:absname("../configs/config")),
    {port, Port} = lists:keyfind(port, 1, Config),
    {listeners, Listeners} = lists:keyfind(listeners, 1, Config),
    Users = gen_users(Listeners),
    [ account:delete(Username) || {Username, _, _} <- Users ],
    {Port, Listeners, Users}.


stop({_Port, _Listeners, Users}) ->
    [ account:delete(Username) || {Username, _, _} <- Users ].


%%====================================================================
%% Actual tests
%%====================================================================

listener_connect({Port, Listeners, _Users}) ->
    NewListeners = 10,
    ChildrenBefore = count_children(listener_sup),
    SocketList = connect_n_times(Port, NewListeners),
    ChildrenDuring = count_children(listener_sup),
    disconnect_all(SocketList),
    ChildrenAfter = count_children(listener_sup),
    [?_assertEqual([{specs, 1}, 
		   {active, Listeners}, 
		   {supervisors, 0}, 
		   {workers, Listeners}], 
		  ChildrenBefore),
     ?_assertEqual([{specs, 1}, 
		   {active, Listeners + NewListeners}, 
		   {supervisors, 0}, 
		   {workers, Listeners + NewListeners}], 
		  ChildrenDuring),
     ?_assertEqual([{specs, 1}, 
		   {active, Listeners}, 
		   {supervisors, 0}, 
		   {workers, Listeners}], 
		  ChildrenAfter)].


listener_register({Port, _Listeners, Users}) ->
    RegisterTrue = register_users(Port, Users, [], ?REGISTER_TRUE),
    RegisterFalse = register_users(Port, Users, [], ?REGISTER_FALSE),
    [RegisterTrue, RegisterFalse].


listener_login_true({Port, Listeners, Users}) ->
    LoginTrue = login_users(Port, Users, [], ?LOGIN_TRUE),
    ListenerChildCount = count_children(listener_sup),
    ClientServChildCount = count_children(client_serv_sup),
    UserCount = length(Users),
    [LoginTrue,
     ?_assertEqual([{specs, 1}, 
		    {active, Listeners}, 
		    {supervisors, 0}, 
		    {workers, Listeners}], 
		   ListenerChildCount),
     ?_assertEqual([{specs, 1}, 
		    {active, UserCount}, 
		    {supervisors, 0}, 
		    {workers, UserCount}], 
		   ClientServChildCount)].


listener_login_false_logged_in({Port, Listeners, Users}) ->
    LoginFalse = login_users(Port, Users, [], ?LOGIN_FALSE_LOGGED_IN),
    ListenerChildCount = count_children(listener_sup),
    ClientServChildCount = count_children(client_serv_sup),
    UserCount = length(Users),
    [LoginFalse,
     ?_assertEqual([{specs, 1}, 
		    {active, Listeners}, 
		    {supervisors, 0}, 
		    {workers, Listeners}], 
		   ListenerChildCount),
     ?_assertEqual([{specs, 1}, 
		    {active, UserCount}, 
		    {supervisors, 0}, 
		    {workers, UserCount}], 
		   ClientServChildCount)].


listener_login_false_password({Port, Listeners, Users}) ->
    UsersFalsePasswords = [ {Username, Password ++ "Oops", Email} || {Username, Password, Email} <- Users ],
    LoginFalse = login_users(Port, UsersFalsePasswords, [], ?LOGIN_FALSE_PASSWORD),
    ListenerChildCount = count_children(listener_sup),
    ClientServChildCount = count_children(client_serv_sup),
    UserCount = length(Users),
    [LoginFalse,
     ?_assertEqual([{specs, 1}, 
		    {active, Listeners}, 
		    {supervisors, 0}, 
		    {workers, Listeners}], 
		   ListenerChildCount),
     ?_assertEqual([{specs, 1}, 
		    {active, UserCount}, 
		    {supervisors, 0}, 
		    {workers, UserCount}], 
		   ClientServChildCount)].


listener_login_false_username({Port, Listeners, Users}) ->
    UsersFalseUsernames = [ {Username ++ "Oops", Password, Email} || {Username, Password, Email} <- Users ],
    LoginFalse = login_users(Port, UsersFalseUsernames, [], ?LOGIN_FALSE_USERNAME),
    ListenerChildCount = count_children(listener_sup),
    ClientServChildCount = count_children(client_serv_sup),
    UserCount = length(Users),
    [LoginFalse,
     ?_assertEqual([{specs, 1}, 
		    {active, Listeners}, 
		    {supervisors, 0}, 
		    {workers, Listeners}], 
		   ListenerChildCount),
     ?_assertEqual([{specs, 1}, 
		    {active, UserCount}, 
		    {supervisors, 0}, 
		    {workers, UserCount}], 
		   ClientServChildCount)].


%%====================================================================
%% Helper functions
%%====================================================================

gen_users(N) ->
    NamePrefix = "ListenerUser",
    Users = [ NamePrefix ++ integer_to_list(Int) || Int <- lists:seq(1, N) ],
    [ {X, X, X} || X <- Users ].


count_children(Supervisor) ->
    timer:sleep(100),
    supervisor:count_children(Supervisor).


connect_n_times(Port, N) when N > 0 ->
    connect_n_times(Port, N, []).

connect_n_times(_Port, 0, Acc) ->
    Acc;
connect_n_times(Port, N, Acc) when N > 0 ->
    {ok, Socket} = client_funs:connect(localhost, Port),
    connect_n_times(Port, N-1, [Socket|Acc]).


disconnect_all(SocketList) ->
    [ client_funs:close(Socket) || Socket <- SocketList ],
    ok.


register_users(_Port, [], Acc, _ExpectedResult) ->
    Acc;
register_users(Port, [User|Users], Acc, ExpectedResult) ->
    {Username, Password, Email} = User,
    {ok, Socket} = client_funs:connect(localhost, Port),
    client_funs:register(Socket, Username, Password, Email),
    receive 
	{tcp, _, Packet} -> 
	    NewAcc = [?_assertEqual(ExpectedResult, Packet) | Acc]; 
	Packet -> 
	    NewAcc = [?_assertEqual(ExpectedResult, Packet) | Acc]
    after
	?TIMEOUT_VALUE ->
	    NewAcc = [?_assertEqual(ExpectedResult, timeout) | Acc]
    end,
    receive
	_ -> ok
    after
	?TIMEOUT_VALUE -> ok
    end,
    register_users(Port, Users, NewAcc, ExpectedResult).


login_users(_Port, [], Acc, _ExpectedResult) ->
    Acc;
login_users(Port, [User|Users], Acc, ExpectedResult) ->
    {Username, Password, _} = User,
    {ok, Socket} = client_funs:connect(localhost, Port),
    client_funs:login(Socket, Username, Password),
    receive 
	{tcp, _, Packet} ->
	    NewAcc = [?_assertEqual(ExpectedResult, Packet) | Acc];
	{tcp_closed, _} ->
	    receive 
		{tcp, _, Packet} ->
		    NewAcc = [?_assertEqual(ExpectedResult, Packet) | Acc]
	    after
		?TIMEOUT_VALUE ->
		    NewAcc = [?_assertEqual(ExpectedResult, timeout_closed) | Acc]
	    end;
	Packet -> 
	    NewAcc = [?_assertEqual(ExpectedResult, Packet) | Acc]	
    after
	?TIMEOUT_VALUE ->
	    NewAcc = [?_assertEqual(ExpectedResult, timeout) | Acc]
    end,
    login_users(Port, Users, NewAcc, ExpectedResult).

