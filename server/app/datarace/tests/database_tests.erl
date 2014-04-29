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
database_inits_test_()->
    {"Testing starting and stopping the database connection.",
      {setup, fun start_init/0, fun stop_init/1, fun ()-> {inorder,[fun init_test/1,
								    fun stop_test/1]}
						 end}}.

database_test_()->
   {"Testing database functions for connecting, query, stopping, ...",
    {setup, fun start/0, fun stop/1, fun ()-> {inorder, [fun db_query_test/1, 
							fun get_row_test/1,
							fun result_to_record_login_test/1,
							fun result_to_record_logout_test/1,
						       fun result_to_record_register_test/1]}
				     end}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_init()->
    application:start(crypto),
    application:start(emysql),
    ok.

stop_init(_)->
    application:stop(emysql),
    application:stop(crypto).


start()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    account:register("AutoTesting", "asdasd167jntyujjngadsdd", "Testing@mail.com"),
    {ok, Id} = account:login("AutoTesting", "asdasd167jntyujjngadsdd"),
    Id.

stop(_)->
    database:db_query("DELETE FROM tUsers WHERE user_name = ""AutoTesting"""),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
init_test(_)->
    ?assert(database:init()).

stop_test(_)->
    ?assert(database:stop()).


db_query_test(_)->
    ?assert(database:db_query("SELECT * FROM tUsers")),
    ?assert(database:db_query(test_sel,"SELECT * FROM tUsers WHERE user_name = ?",["AutoTesting"])).


get_row_test(_)->
    ?assertEqual(database:get_row([1,2,3],1),1),
    ?assertEqual(database:get_row([],1),{error, no_item}).
    
result_to_record_login_test(Userid)->
    Sql_result = database:db_query(login_user_info, 
			    <<"SELECT id, salt, password from tUsers WHERE user_name = ?">>,
				   ["AutoTesting"]),
    Record = database:result_to_record(Sql_result, login_table),
    ?assertEqual(Record#login_table.id, Userid).


result_to_record_logout_test(Userid)->
    Sql_result = database:db_query(loginlog_logout_select,
				<<"SELECT max(id) as id FROM tLoginLog WHERE userId = ?">>,
				   [Userid]),
    Record = database:get_row(database:result_to_record(Sql_result, loginlog_table),1),
    ?assert(Record#loginlog_table.id).


result_to_record_register_test(Userid)->
    Sql_result = database:db_query(register_select,
				   <<"SELECT id FROM tUsers WHERE user_name = ?">>,
				   ["AutoTesting"]),
    Record = database:result_to_record(Sql_result, register_table),
    ?assertEqual(Record#register_table.id, Userid).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

