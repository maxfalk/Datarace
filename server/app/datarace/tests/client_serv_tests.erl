
-module(client_serv_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/types.hrl").


%%====================================================================
%% Test description
%%====================================================================

client_serv_test_()-> 
    {"Test client_serv functionality: initialization, request, request lookup, search, get home stats, get history, start match, run match, stop match, log out", 
      {setup, 
       fun start/0, 
       fun stop/1, 
       fun (SetupData) -> 
	       [client_serv_init(SetupData), 
		client_serv_request(SetupData), 
		client_serv_request_lookup(SetupData), 
		client_serv_search_string(SetupData), 
		client_serv_get_home_stats(SetupData),
		client_serv_get_history(SetupData),
		client_serv_start_match(SetupData), 
		client_serv_run_match(SetupData),
		client_serv_stop_match(SetupData),
		client_serv_logout(SetupData)] 
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
    [ account:delete(UID) || {_,_,UID} <- LoggedInUsers ].


%%====================================================================
%% Actual tests
%%====================================================================

client_serv_init({NoUsers, LoggedInUsers}) ->
    Asserts = [[?_assert(not is_atom(Socket)), 
		?_assert(is_integer(UID))] || 
		  {Socket,_,UID} <- LoggedInUsers ],
    Children = count_children(client_serv_sup),
    [Asserts, 
     ?_assertEqual([{specs, NoUsers}, 
		    {active, NoUsers}, 
		    {supervisors, 0}, 
		    {workers, NoUsers}], 
		   Children) ].


client_serv_request({_NoUsers, LoggedInUsers}) ->
    ExpectedRequests = make_requests(LoggedInUsers),
    Requests = [element(3,lists:last(element(1, usercom:request_lookup(UID)))) || 
		   {_,_,UID} <- LoggedInUsers],
    lists:zipwith(fun (E, R) -> ?_assertEqual(E, R) end, ExpectedRequests, Requests).


client_serv_request_lookup({_NoUsers, LoggedInUsers}) ->
    [ make_request_lookup(User) || User <- LoggedInUsers].


client_serv_get_home_stats({_NoUsers, LoggedInUsers}) ->
    [ get_home_stats(User) || User <- LoggedInUsers ].


client_serv_get_history({_NoUsers, LoggedInUsers}) ->
    [ get_history(User) || User <- LoggedInUsers ].


client_serv_search_string({NoUsers, LoggedInUsers}) ->
    [ search_string(NoUsers, User) || User <- LoggedInUsers ].


client_serv_start_match({_NoUsers, LoggedInUsers}) ->
    [ start_match(User) || User <- LoggedInUsers ].


client_serv_run_match({_NoUsers, LoggedInUsers}) ->
    [ gps_match(User) || User <- LoggedInUsers ],
    [ pos_match(User) || User <- LoggedInUsers ].


client_serv_stop_match({_NoUsers, LoggedInUsers}) ->
    [ stop_match(User) || User <- LoggedInUsers ].


client_serv_logout({_NoUsers, LoggedInUsers}) ->
    timer:sleep(100),
    [ logout(User) || User <- LoggedInUsers ],
    Children = count_children(client_serv_sup),
    [ ?_assertEqual([{specs, 0}, 
		     {active, 0}, 
		     {supervisors, 0}, 
		     {workers, 0}], 
		    Children)].


%%====================================================================
%% Helper functions
%%====================================================================

gen_users(N) ->
    NamePrefix = "ClientServBosse",
    [ NamePrefix ++ integer_to_list(Int) || Int <- lists:seq(1, N) ].


count_children(Supervisor) ->
    timer:sleep(100),
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
			  timer:sleep(100),
			  client_funs:request(Socket, UID, 1),
			  UID
		  end,
		  LoggedInUsers, lists:reverse(LoggedInUsers)).


make_request_lookup({Socket, _,_}) ->
    client_funs:request_lookup(Socket),
    Made = receive
	       {tcp, _, Packet1} -> Packet1
	   after
	       1000 -> <<0>>
	   end,
    Chal = receive
	       {tcp, _, Packet2} -> Packet2
	   after
	       1000 -> <<0>>
	   end,
    [?_assertEqual(92, byte_size(Made)), ?_assertEqual(92, byte_size(Chal))].


get_home_stats({Socket, _,_}) ->
    client_funs:get_home_stats(Socket),
    HomeStats = receive 
		    {tcp, _, Packet} -> Packet
		after
		    1000 -> <<0>>
		end,
    ?_assertEqual(80, byte_size(HomeStats)).


search_string(_NoUsers, {Socket, _,_}) ->
    client_funs:search_string(Socket, "ClientServBosse"),
    SearchResults = receive
			{tcp, _, Packet} -> Packet
		    after
			1000 -> <<0>>
		    end,
    ?_assertEqual(2+54*5, byte_size(SearchResults)).


start_match({Socket, _, UID}) ->
    RequestId = get_request_id(UID),
    client_funs:request_accept(Socket, RequestId),
    client_funs:start_match(Socket, RequestId),
    receive 
	{tcp, _, Packet} -> 
	    ?_assertEqual(?MATCH_CONFIRM, Packet)
    after
	5000 -> 
	    ?_assertEqual(true, timeout)
    end.


gps_match({Socket, _,_}) ->
    client_funs:gps_match(Socket, 0.0, 0.0).


pos_match({Socket, _,_}) ->
    client_funs:pos_match(Socket),
    receive
	{tcp, _, Packet} ->
	    ?_assertEqual(10, byte_size(Packet))
    after
	1000 -> 
	    ?_assertEqual(true, timeout)
    end.


get_history({Socket, _,_}) ->
    client_funs:get_history(Socket),
    receive
	{tcp, _, Packet} ->
	    ?_assertEqual(?GET_HISTORY_REPLY, Packet)
    after
	1000 ->
	    ?_assertEqual(true, timeout)
    end.


stop_match({Socket, _,_}) ->
    client_funs:stop_match(Socket),
    receive
	{tcp, _, Packet} ->
	    ?_assertEqual(30, byte_size(Packet))
    after
	2000 ->
	    ?_assertEqual(true, timeout)
    end.


get_request_id(UID) ->
    {_Made, Chal} = usercom:request_lookup(UID),
    {request_table, RequestId, _,_,_,_,_} = hd(Chal),
    RequestId.


logout({Socket, _,_}) ->
    client_funs:logout(Socket).
