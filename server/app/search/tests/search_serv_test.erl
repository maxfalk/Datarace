%% @doc Tests for search_serv module

-module(search_serv_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/database.hrl").


%%%%%%%%%%%%%%%%%%%%%%%
%% TEST DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%

search_serv_test_()->
    {"Test search serv functions",
      {setup, fun setup/0, fun stop/1,
      fun (SetupData)->
	      [start_stop_test(SetupData),
	      search_test(SetupData)]
      end}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SETUP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup()->
    application:start(crypto),
    application:start(emysql),
    application:start(datarace),
    application:start(search),
    account:register("search_test0","search_test0","search_test0@mail.com"),
    account:register("search_test1","search_test1","search_test1@mail.com"),
    {account:login("search_test0","search_test0"),
     account:login("search_test1","search_test1")}.



stop({U1, U2})->
    account:delete(U1),
    account:delete(U2),
    application:stop(search),    
    application:stop(datarace),
    application:stop(emysql),
    application:stop(crypto).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACTUAL TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test(_)->
    {ok, Pid} = search_sup:start_search_serv(),
    search_serv:stop(Pid, stop).


search_test(_)->
    {ok, Pid} = search_sup:start_search_serv(),
    User0 = hd(search_serv:search(Pid, "search_test0")),
    User1 = hd(search_serv:search(Pid, "search_test1")),
    User2 = search_serv:search(Pid, "search_test"),
    [?_assertEqual(User0#user_search_table.userName, "search_test0"),
    ?_assertEqual(User1#user_search_table.userName, "search_test1"),
    ?_assertEqual(length(User2),2)].
    

%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%
