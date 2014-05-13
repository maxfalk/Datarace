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
    {setup, fun start/0, fun stop/1,
     fun (Data) ->
	     [db_register(Data),
	     login(Data),
	     logout(Data)]
     end}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    account:register("Autotest","Autotest","AT@mail.com").

stop(_)->
    {ok, Id} = account:login("Autotest","Autotest"),
    account:logout(Id),
    account:delete(Id),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).

    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

db_register(RegOut)->
    [?_assertEqual(RegOut, ok),
     ?_assertEqual(account:register("Autotest","o","testar"), {error, user_already_exist})].

login(_)-> 
    [?_assert(account:login("Autotest","Autotest") =/= {error, already_loggedin}),
     ?_assertEqual(account:login("Autotest","Autotest"),{error, already_loggedin}),
     ?_assertEqual(account:login("qpqpqp","test"), {error,no_user}),
     ?_assertEqual(account:login("Autotest","ost"), {error,wrong_password})].

logout(_)->
    ok = account:logout("Autotest"),
    {ok, Id} = account:login("Autotest","Autotest"),
    [?_assertEqual(account:logout(Id), ok),
    ?_assertEqual(account:logout(77), ok)].


    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


