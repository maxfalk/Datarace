%%@doc Tests for the usercom module
%%
%%
%%

-module(usercom_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/database.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
request_test_()->
    {"Test request function",
     {setup, fun start_request/0, fun stop_request/1, 
      fun (SetupData)->
	      request(SetupData)
      end}}. 

request_util_test_()->
      {"Test request functions lookup, accept and cancel",
      {setup, fun start_request_util/0, fun stop_request_util/1, 
       fun (SetupData)->
		[request_lookup(SetupData),
		request_accept(SetupData),
		request_cancel(SetupData)]
       end}}.


match_test_()->
    {"Test Match functions",
      {setup, fun start_match/0, fun stop_match/1, 
       fun (SetupData)->
		[match(SetupData),
		 set_winner(SetupData),
		match_stop(SetupData)]
       end}}.


gps_test_()->
    {"Test gps functions",
     {setup, fun start_gps/0, fun stop_gps/1, 
      fun (SetupData)->
	      [gps_save(SetupData)]
      end}}.


stats_test_()->
    {"Test stats functions",
     {setup, fun start_stats/0, fun stop_stats/1, 
      fun (SetupData)->
	      [stats_home(SetupData),
	       get_num_pending_requests(SetupData),
	      get_history(SetupData),
	      get_match_end_stats(SetupData)]
      end}}.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Request setup cleanup
start_request()->
    create_users().

stop_request({U1, U2})->
    delete_users({U1, U2}).


%% Request util setup cleanup
start_request_util()->
    {U1, U2} = create_users(),
    timer:sleep(100),
    usercom:request(U1, U2, 23),
    timer:sleep(100),
    {U1, U2}.

stop_request_util({U1, U2})->
    delete_users({U1, U2}).


%% Match setup cleanup
start_match()->
    {U1, U2} = create_users(),
    timer:sleep(100),
    usercom:request(U1, U2, 23),
    timer:sleep(100),
    {R, _R1} = usercom:request_lookup(U1),
    Result = hd(R), 
    usercom:match(Result#request_table.id),
    {Result#request_table.id, U1, U2}.

stop_match({_ , U1, U2})->
    delete_users({U1, U2}).


%% Gps setup cleanup
start_gps()->
    {U1, U2} = create_users(),
    usercom:request(U1, U2, 23),
    timer:sleep(100),
    {R, _R1} = usercom:request_lookup(U1),
    Result = hd(R),
    Result2 = usercom:match(Result#request_table.id),
    {Result2#match_table.id, U1, U2}.   

    


stop_gps({_, U1, U2})->
    delete_users({U1, U2}).
    

%%Stats setup, cleanup

start_stats()->
    {U1, U2} = create_users(),
    usercom:request(U1, U2, 1),
    timer:sleep(100),

    %% Player 1
    {R_U1, _} = usercom:request_lookup(U1),
    Result_U1 = hd(R_U1),
    timer:sleep(100),
    Result2_U1 = usercom:match(Result_U1#request_table.id),
    timer:sleep(100),
    usercom:gps_save(U1, Result2_U1#match_table.id, 2345678.4567890, 5678.5678),
    timer:sleep(100),
    usercom:gps_save(U1, Result2_U1#match_table.id, 2345678.4567890, 6678.5678),
    timer:sleep(100),
    usercom:match_stop(U1, Result2_U1#match_table.id, Result_U1#request_table.id),

    %% Player 2
    {_, R1_U2} = usercom:request_lookup(U2),
    Result_U2 = hd(R1_U2),
    usercom:request_accept(Result_U2#request_table.id),   
    timer:sleep(100),
    Result2_U2 = usercom:match(Result_U2#request_table.id),
    timer:sleep(100),
    usercom:gps_save(U2, Result2_U2#match_table.id, 2345678.4567890, 5678.5678),
    timer:sleep(1000),
    usercom:gps_save(U2, Result2_U2#match_table.id, 2345678.4567890, 6678.5678),
    timer:sleep(100),
    usercom:match_stop(U2, Result2_U2#match_table.id, Result_U2#request_table.id),
    {U1, U2, <<"test_usercom1">>, <<"test_usercom2">>,  Result2_U2#match_table.id}.



stop_stats({U1, U2, _Username, _, _})->
    delete_users({U1, U2}).    




%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Test requests
request({U1, U2})->
    timer:sleep(100),
    ?_assertEqual(ok, usercom:request(U1, U2, 23)).

request_lookup({U1, U2}) ->
    {R, _} = usercom:request_lookup(U1),
    Result = hd(R),
    [?_assertEqual(Result#request_table.challenged_userId, U2),
     ?_assertEqual(Result#request_table.state, 0),
     ?_assertEqual(Result#request_table.distance, 23)].

request_accept({_U1, U2})->
    {_, R1} = usercom:request_lookup(U2),
    Result = hd(R1),   
    usercom:request_accept(Result#request_table.id),
    timer:sleep(100),
    {_, R3} = usercom:request_lookup(U2),
    Result1 = hd(R3),   
    [?_assertEqual(Result1#request_table.state, 1)].

request_cancel({_U1, U2})->
    {_, R1} = usercom:request_lookup(U2),
    Result = hd(R1),  
    usercom:request_cancel(Result#request_table.id),
    timer:sleep(100),
    {_, R3} = usercom:request_lookup(U2),
    [?_assertEqual(length(R3), 0)].
    
%%Test match    

match({UserRequestId, _, U2})->
    Result = usercom:match(UserRequestId),
    [?_assertEqual(Result#match_table.userId, U2),
    ?_assertEqual(Result#match_table.requestId, UserRequestId)].

    
set_winner({RequestId, U1, _})->
    Result = usercom:match(RequestId),
    usercom:set_winner(Result#match_table.id, U1),
    timer:sleep(100),
    Result1 = usercom:match(RequestId),
    ?_assertEqual(Result1#match_table.winner, U1).


match_stop({UserRequestId, U1, _})->
    Result = usercom:match(UserRequestId),
    usercom:match_stop(U1, Result#match_table.id, UserRequestId),
    timer:sleep(100),
    Result1 = usercom:match(UserRequestId),
    [?_assertEqual(Result1#match_table.id, Result#match_table.id)].



%%Test gps
gps_save({MatchId, U1, _U2})->
    usercom:gps_save(U1, MatchId, 2345678.4567890, 5678.5678),
    timer:sleep(100),
    Result = usercom:gps_get(U1, MatchId),
    [?_assertEqual(length(Result), 1)].
   


%%Test stats

stats_home({UserId, _U2, UserName, _, _})->
    database:db_query("CALL insert_user_stats()"),
    timer:sleep(100),
    Result = usercom:get_home_stats(UserId),
    Result1 = usercom:get_home_stats(-1),
    [?_assertEqual(Result#user_stats_table.userName, UserName),
     ?_assertEqual(Result#user_stats_table.averageDistance, 0),
     ?_assertEqual(Result#user_stats_table.averageSpeed, 0),
     ?_assertEqual(Result#user_stats_table.wins, 0),
     ?_assertEqual(Result#user_stats_table.matches, 0),
     ?_assertEqual(Result#user_stats_table.requests, 1),
     
     ?_assertEqual(Result1#user_stats_table.userName, <<"">>),
     ?_assertEqual(Result1#user_stats_table.averageDistance, 0),
     ?_assertEqual(Result1#user_stats_table.averageSpeed, 0),
     ?_assertEqual(Result1#user_stats_table.wins, 0),
     ?_assertEqual(Result1#user_stats_table.matches, 0),
     ?_assertEqual(Result1#user_stats_table.requests, 0)].


get_num_pending_requests({U1, UserId, _, _, _})->
    usercom:request(U1, UserId, 2),
    timer:sleep(100),
    [?_assertEqual(usercom:get_num_pending_requests(UserId),1),
     ?_assertEqual(usercom:get_num_pending_requests(-1),0)].


get_history({_U1, UserId, UserName, _, _})->
    Result = hd(usercom:get_history(UserId)),
    [?_assertEqual(Result#match_stats_table.userId, UserId),
     ?_assertEqual(Result#match_stats_table.userName, UserName),
     ?_assert(Result#match_stats_table.time > 0),
     ?_assert(Result#match_stats_table.winner > 0),
     ?_assertEqual(Result#match_stats_table.distance, 1),
     ?_assert(Result#match_stats_table.averageSpeed > 0),
     ?_assertEqual(Result#match_stats_table.state, 1)].



get_match_end_stats({_, UserId, _, UserName2, Matchid})->
    Result = hd(tl(usercom:get_match_end_stats(Matchid))),
    [?_assertEqual(Result#match_stats_table.userId, UserId),
     ?_assertEqual(Result#match_stats_table.userName, UserName2),
     ?_assert(Result#match_stats_table.time > 0),
     ?_assert(Result#match_stats_table.winner > 0),
     ?_assertEqual(Result#match_stats_table.distance, 1),
     ?_assert(Result#match_stats_table.averageSpeed > 0),
     ?_assertEqual(Result#match_stats_table.state, 1)].


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

create_user(Username, Password)->
    ok = account:register(Username,Password, Username ++ "@mail.com"),
    timer:sleep(100), 
    {ok, U1} = account:login(Username, Password),
    timer:sleep(100),
    account:logout(U1),
    timer:sleep(100),
    U1.

create_users()->
    account:delete("test_usercom1"),
    account:delete("test_usercom2"),
    U1 = create_user("test_usercom1","oaisdnni123asd"),
    timer:sleep(100),
    U2 = create_user("test_usercom2", "os09mlsni123asd"),
    {U1, U2}.
 
delete_users({U1, U2})->
    account:delete(U1),
    account:delete(U2).
