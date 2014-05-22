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
			 result_to_record_register(SetupData)]
     end}}.




%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


start()->
    account:delete("AutoTesting"),
    account:register("AutoTesting", "sdfgaudya8s72","Testing@mail.com"),
    timer:sleep(100),
    {ok, Id} = account:login("AutoTesting","sdfgaudya8s72"),
    Id.

stop(Id)->
    account:delete("AutoTesting").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actuall tests      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%



db_query(_)->
    {R, _, _, _, _} = database:db_query("SELECT * FROM tUsers"),
    {R1, _, _, _, _} = database:db_query(test_sel,
			 "SELECT * FROM tUsers WHERE userName = ?",["AutoTesting"]),
    [?_assertEqual(result_packet, R),
    ?_assertEqual(result_packet, R1)].


get_row(_)->
    [?_assertEqual(database:get_row([1,2,3],1),1),
    ?_assertEqual(database:get_row([],1),{error, no_item})].
    
result_to_record_login(Id)->
    Sql_result = database:db_query(login_user_info, 
			    <<"SELECT id, salt, password from tUsers WHERE userName = ?">>,
				   ["AutoTesting"]),
    Record = hd(database:result_to_record(Sql_result, login_table)),
    [?_assertEqual(Record#login_table.id, Id)].


result_to_record_logout(Id)->
    Sql_result = database:db_query(loginlog_logout_select,
				<<"SELECT t2.id as id 
                                   FROM tLoginLog t1 inner join 
                                        tUsers t2 on t1.userId = t2.id  
                                   WHERE t2.userName = ?">>,
				   ["AutoTesting"]),
    Record = database:get_row(database:result_to_record(Sql_result, loginlog_table),1),
    [?_assertEqual(Record#loginlog_table.id, Id)].

result_to_record_register(Id)->
    Sql_result = database:db_query(register_select,
				   <<"SELECT id FROM tUsers WHERE userName = ?">>,
				   ["AutoTesting"]),
    Record = hd(database:result_to_record(Sql_result, register_table)),
    [?_assertEqual(Record#register_table.id, Id)].


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

