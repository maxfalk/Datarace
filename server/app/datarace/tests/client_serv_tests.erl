
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
	       [client_serv_init(SetupData),
		client_serv_request(SetupData),
		client_serv_request_lookup(SetupData)]
       end}}.


%%====================================================================
%% Setup functions
%%====================================================================

start() ->
    NoUsers = 10,
    Users = gen_users(NoUsers),
    {ok, Config} = file:consult(filename:absname("../configs/config")),
    {port, Port} = lists:keyfind(port, 1, Config),
    [ account:delete(Username) || Username <- Users ],
    [ account:register(Username, Username, Username) || Username <- Users ],
    LoggedInUsers = [ login_user(Port, User) || User <- Users ],
    {NoUsers, LoggedInUsers}.


stop({_NoUsers, LoggedInUsers}) ->
    [ account:delete(Username) || {Username,_,_UID} <- LoggedInUsers ].


%%====================================================================
%% Actual tests
%%====================================================================

client_serv_init({NoUsers, LoggedInUsers}) ->
    [[[?_assert(not is_atom(Socket)), ?_assert(is_integer(UID))] || 
	 {Socket,_,UID} <- LoggedInUsers ],
     [ ?_assertEqual([{specs, NoUsers}, 
		      {active, NoUsers}, 
		      {supervisors, 0}, 
		      {workers, NoUsers}], 
		     count_children(client_serv_sup)) ]].


client_serv_request({_NoUsers, LoggedInUsers}) ->
    ExpectedRequests = make_requests(LoggedInUsers),
    Requests = [element(3,lists:last(element(1, usercom:request_lookup(UID)))) || 
		   {_,_,UID} <- LoggedInUsers],
    lists:zipwith(fun (E, R) -> ?_assertEqual(E, R) end, ExpectedRequests, Requests).


client_serv_request_lookup({_NoUsers, LoggedInUsers}) ->
    [ make_request_lookup(User) || User <- LoggedInUsers].


%%====================================================================
%% Helper functions
%%====================================================================

gen_users(N) ->
    NamePrefix = "ClientServBosse",
    [ NamePrefix ++ integer_to_list(Int) || Int <- lists:seq(1, N) ].


count_children(Supervisor) ->
    supervisor:count_children(Supervisor).


login_user(Port, Username) ->
    [{register_table, UID}] = account:get_user(Username),
    {ok, Socket} = client_funs:connect(localhost, Port),
    client_funs:login(Socket, Username, Username),
    receive
	{tcp, _, ?LOGIN_TRUE} -> 
	    {Socket, Username, UID};
	{tcp_closed, _Msg} ->
	    receive 
		{tcp, _, _} -> 
		    {tcp_closed, Username, UID}
	    after
		1000 ->
		    timeout
	    end;
	_ -> 
	    {tcp_error, Username, UID}
    after
	1000 ->
	    timeout
    end.

  
make_requests(LoggedInUsers) ->
    lists:zipwith(fun({Socket,_,_}, {_,_,UID}) ->
			  timer:sleep(10),
			  client_funs:request(Socket, UID, 1),
			  UID
		  end,
		  LoggedInUsers, lists:reverse(LoggedInUsers)).


make_request_lookup({Socket, _,_}) ->
    client_funs:request_lookup(Socket),
    Made = receive
	       {tcp, _, Packet1} -> Packet1
	   after
	       1000 -> timeout
	   end,
    Chal = receive
	       {tcp, _, Packet2} -> Packet2
	   after
	       1000 -> timeout
	   end,
    [?_assertEqual(92, byte_size(Made)), ?_assertEqual(92, byte_size(Chal))].

