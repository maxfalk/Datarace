%%Tests for the account module.
%%
%%
%%
-module(account_tests).

-include_lib("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%{setup, Where, Setup, Cleanup, Instantiator} 
account_test_()->
   {"Test registering, login and logout directly at the DB",
    {setup, fun start/0, fun stop/1,fun ()-> {inorder, [fun register_test/1,
							   fun login_test/1, 
							   fun logout_test/1]}
						    end}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    ok.

stop(_)->
    database:db_query("DELETE FROM tUsers WHERE user_name = ""Autotest"""),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).

    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


login_test(_)->
    L1 = emysql:as_proplist(database:db_query(
			      "SELECT id FROM tUsers WHERE user_name = ""Autotest""")),
    Id = hd(L1),
    ?assertEqual(account:login("Autotest","Autotest"), {ok, Id}),
    ?assertEqual(account:login("qpqpqp","test"), {error, no_user}),
    ?assertEqual(account:login("Autotest","ost"), {error, wrong_password}).

logout_test(_)->
    {ok, Id} = account:login("Autotest","Autotest"),
    ?assertEqual(account:logout(Id), ok),
    ?assertEqual(account:logout(77), ok).

register_test(_)->
    ?assertEqual(account:register("Autotest","Autotest","AT@mail.com"), ok),
    ?assertEqual(account:register("Attotest","o","testar",""), {error, user_already_exist}).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


