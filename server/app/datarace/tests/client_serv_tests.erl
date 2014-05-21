
-module(client_serv_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/types.hrl").


%%====================================================================
%% Test description
%%====================================================================

client_serv_test_()->
    {"Test client_serv functionality: lots...",
      {setup, 
       fun start/0, 
       fun stop/1, 
       fun (SetupData) ->
	       [client_serv_init(SetupData)]
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
    [ account:register(Username, Password, Email) || {Username, Password, Email} <- Users ],
    LoggedInUsers = [ login_user(Port, User) || User <- Users ],
    {Port, Listeners, LoggedInUsers}.


stop({_Port, _Listeners, LoggedInUsers}) ->
    [ account:delete(Username) || {_, Username, _, _} <- LoggedInUsers ].


%%====================================================================
%% Actual tests
%%====================================================================

client_serv_init({_Port, _Listeners, LoggedInUsers}) ->
    [ ?_assert(not is_atom(Socket)) || {Socket,_,_,_} <- LoggedInUsers ].


client_serv_({_Port, _Listeners, LoggedInUsers}) ->
    [].


%%====================================================================
%% Helper functions
%%====================================================================

gen_users(N) ->
    NamePrefix = "ClientServUser",
    Users = [ NamePrefix ++ integer_to_list(Int) || Int <- lists:seq(1, N) ],
    [ {X, X, X} || X <- Users ].


count_children(Supervisor) ->
    supervisor:count_children(Supervisor).


login_user(Port, {Username, Password, Email}) ->
    {ok, Socket} = client_funs:connect(localhost, Port),
    client_funs:login(Socket, Username, Password),
    receive
	{tcp, _, ?LOGIN_TRUE} -> 
	    {Socket, Username, Password, Email};
	{tcp_closed, _Msg} ->
	    receive 
		{tcp, _, Packet} -> 
		    {tcp_closed, Username, Password, Email}
	    end;
	Result -> 
	    {tcp_error, Username, Password, Email}
    end.
