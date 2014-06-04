%%Tests for the account module.
%%
%%
%%
-module(database_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/database.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%{setup, Where, Setup, Cleanup, Instantiator} 
database_test_()->
   {"Testing database functions for connecting, query, stopping, ...",
    {setup, fun start/0, fun stop/1, 
     fun (SetupData)-> 
			[db_query(SetupData), 
			 get_row(SetupData),
			 result_to_record_login(SetupData),
			 result_to_record_logout(SetupData),
			 result_to_record_register(SetupData),
			 result_to_record_request(SetupData),
			 result_to_record_match(SetupData),
			 result_to_record_gps(SetupData),
			 result_to_record_request_info(SetupData),
			 result_to_record_user_search(SetupData),
			 result_to_record_user_stats(SetupData),
			 result_to_record_match_stats(SetupData)]
     end}}.




%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


start()->
    account:delete("AutoTesting"),
    account:register("AutoTesting", "sdfgaudya8s72","Testing@mail.com"),
    timer:sleep(300),
    {ok, Id} = account:login("AutoTesting","sdfgaudya8s72"),
    Id.

stop(_)->
    account:delete("AutoTesting").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actuall tests      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%



db_query(_)->
    {R, _, _, _, _} = database:db_query("SELECT * FROM tUsers"),
    {R1, _, _, _, _} = database:db_query(test_sel,
			 "SELECT * FROM tUsers WHERE userName = ? and state = 0",["AutoTesting"]),
    [?_assertEqual(result_packet, R),
    ?_assertEqual(result_packet, R1)].


get_row(_)->
    [?_assertEqual(database:get_row([1,2,3],1),1),
    ?_assertEqual(database:get_row([],1),{error, no_item})].
    
result_to_record_login(Id)->
    Sql_result = database:db_query(login_user_info, 
			    <<"SELECT id, salt, password from tUsers 
                               WHERE userName = ? and state = 0">>,
				   ["AutoTesting"]),
    Record = hd(database:result_to_record(Sql_result, login_table)),
    [?_assertEqual(Record#login_table.id, Id)].


result_to_record_logout(Id)->
    Sql_result = database:db_query(loginlog_logout_select,
				<<"SELECT t2.id as id 
                                   FROM tLoginLog t1 inner join 
                                        tUsers t2 on t1.userId = t2.id  
                                   WHERE t2.userName = ? and t2.state = 0">>,
				   ["AutoTesting"]),
    Record = database:get_row(database:result_to_record(Sql_result, loginlog_table),1),
    [?_assertEqual(Record#loginlog_table.id, Id)].

result_to_record_register(Id)->
    Sql_result = database:db_query(register_select,
				   <<"SELECT id FROM tUsers WHERE userName = ? and state = 0">>,
				   ["AutoTesting"]),
    Record = hd(database:result_to_record(Sql_result, register_table)),
    [?_assertEqual(Record#register_table.id, Id)].


result_to_record_request(Id)->
      Sql_result = database:db_query(request_select_made, 
				   <<"SELECT t1.id, 0 as challenged_userId, 
                                             t1.userName as user_name, 
                                             now() as date, 2 as state, 3 as distance
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				     [Id]),
    Record = hd(database:result_to_record(Sql_result, request_table)),
    [?_assertEqual(Record#request_table.id, Id),
     ?_assertEqual(Record#request_table.challenged_userId, 0),
     ?_assertEqual(Record#request_table.user_name, <<"AutoTesting">>),
     ?_assertEqual(Record#request_table.state, 2),
     ?_assertEqual(Record#request_table.distance, 3),
     ?_assert(Record#request_table.date =/= <<"undefined">>)].

result_to_record_match(Id)->
    Sql_result = database:db_query(match_select_test, 
				   <<"SELECT t1.id, 0 as userId, 1 as winner, 2 as requestId
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				   [Id]),
    Record = hd(database:result_to_record(Sql_result, match_table)),
    [?_assertEqual(Record#match_table.id, Id),
     ?_assertEqual(Record#match_table.userId, 0),
     ?_assertEqual(Record#match_table.winner, 1),
     ?_assertEqual(Record#match_table.requestId, 2)].


result_to_record_user_stats(Id)->
    Sql_result = database:db_query(user_stats_select_test, 
				   <<"SELECT t1.userName, 0 as averageSpeed, 1 as averageDistance, 
                                              2 as wins, 3 as matches, 4 as requests
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				   [Id]),
    Record = hd(database:result_to_record(Sql_result, user_stats_table)),
    [?_assertEqual(Record#user_stats_table.userName, <<"AutoTesting">>),
     ?_assertEqual(Record#user_stats_table.averageSpeed, 0),
     ?_assertEqual(Record#user_stats_table.averageDistance, 1),
     ?_assertEqual(Record#user_stats_table.wins, 2),
     ?_assertEqual(Record#user_stats_table.matches, 3),
     ?_assertEqual(Record#user_stats_table.requests, 4)].


result_to_record_gps(Id)->
    Sql_result = database:db_query(gps_select_test, 
				   <<"SELECT 2345.6234 as longitude, 7897.12321 as latitude, 
                                             now() as time
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				   [Id]),
    Record = hd(database:result_to_record(Sql_result, gps_table)),
    [?_assertEqual(Record#gps_table.longitude, 2345.6234),
     ?_assertEqual(Record#gps_table.latitude, 7897.12321),
     ?_assert(Record#gps_table.time =/= <<"undefined">>)].


result_to_record_request_info(Id)->
    Sql_result = database:db_query(gps_select_test, 
				   <<"SELECT 1 as requestId 
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				   [Id]),
    Record = hd(database:result_to_record(Sql_result, request_info_table)),
    [?_assertEqual(Record#request_info_table.requestId, 1)].


result_to_record_user_search(Id)->
    Sql_result = database:db_query(user_search_select_test, 
				   <<"SELECT t1.id, t1.userName 
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				   [Id]),
    Record = hd(database:result_to_record(Sql_result, user_search_table)),
    [?_assertEqual(Record#user_search_table.id, Id),
     ?_assertEqual(Record#user_search_table.userName, <<"AutoTesting">>)].


result_to_record_match_stats(Id)->
    Sql_result = database:db_query(match_stats_select_test, 
				   <<"SELECT t1.id as userId, t1.userName, now() as time, 1 as winner,
                                             2 as distance, 3.0 as averageSpeed, 4 as state
                                       FROM                               
                                        tUsers t1
                                      WHERE
                                       t1.id = ?">>,
				   [Id]),
    Record = hd(database:result_to_record(Sql_result, match_stats_table)),
    [?_assertEqual(Record#match_stats_table.userId, Id),
     ?_assertEqual(Record#match_stats_table.userName, <<"AutoTesting">>),
     ?_assert(Record#match_stats_table.time =/= <<"undefined">>),
     ?_assertEqual(Record#match_stats_table.winner, 1),
     ?_assertEqual(Record#match_stats_table.distance, 2),
     ?_assertEqual(Record#match_stats_table.averageSpeed, 3.0),
     ?_assertEqual(Record#match_stats_table.state, 4)].



%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

