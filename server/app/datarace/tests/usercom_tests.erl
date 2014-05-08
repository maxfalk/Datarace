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
		[create_match(SetupData),
		 get_match(SetupData),
		 new_match(SetupData),
		 set_winner(SetupData)]
       end}}.


gps_test_()->
    {"Test gps functions",
     {setup, fun start_gps/0, fun stop_gps/1, 
      fun (SetupData)->
	      [gps_save(SetupData)]
      end}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Request setup cleanup
start_request()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    create_users().

stop_request({U1, U2})->
    delete_users({U1, U2}),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).

%% Request util setup cleanup
start_request_util()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    {U1, U2} = create_users(),
    usercom:request(U1, U2, 23),
    {U1, U2}.

stop_request_util({U1, U2})->
    delete_users({U1, U2}),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).

%% Match setup cleanup
start_match()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    {U1, U2} = create_users(),
    usercom:request(U1, U2, 23),
    Result = hd(usercom:request_lookup(U1)),
    usercom:create_match(Result#request_table.id, U1, U2),
    {Result#request_table.id, U1, U2}.

stop_match({_ , U1, U2})->
    delete_users({U1, U2}),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).

%% Gps setup cleanup
start_gps()->
    application:start(crypto),
    application:start(emysql),
    database:init(),
    {U1, U2} = create_users(),
    usercom:request(U1, U2, 23),
    Result = hd(usercom:request_lookup(U1)),
    Result2 = hd(usercom:new_match(Result#request_table.id, U1, U2)),
    {Result2#match_table.id, U1, U2}.   

    


stop_gps({_, U1, U2})->
    delete_users({U1, U2}),
    database:stop(),
    application:stop(emysql),
    application:stop(crypto).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Test requests
request({U1, U2})->
    ?_assertEqual(ok, usercom:request(U1, U2, 23)).

request_lookup({U1, U2})->
    Result = hd(usercom:request_lookup(U1)),
    [?_assertEqual(Result#request_table.challenged_userId, U2),
    ?_assertEqual(Result#request_table.state, 0)].

request_accept({U1, U2})->
    Result = hd(usercom:request_lookup(U1)),
    usercom:request_accept(Result#request_table.id),
    Result_after = hd(usercom:request_lookup(U1)),
    ?_assertEqual(Result_after#request_table.state, 1).

request_cancel({U1, U2})->
    Result = hd(usercom:request_lookup(U1)),
    usercom:request_cancel(Result#request_table.id),
    Result_after = hd(usercom:request_lookup(U1)),
    ?_assertEqual(Result_after#request_table.state, 2).
    
%%Test match    

new_match({RequestId, U1, U2})->
    Result = hd(usercom:new_match(RequestId, U1, U2)),
    [?_assertEqual(Result#match_table.main_userId, U1),
    ?_assertEqual(Result#match_table.sec_userId, U2),
    ?_assertEqual(Result#match_table.requestId, RequestId)].

create_match({RequestId, U1, U2})->
    ?_assertEqual(ok, usercom:create_match(RequestId, U1, U2)).

get_match({RequestId, U1, U2})->
    Result = hd(usercom:get_match(RequestId)),
    [?_assertEqual(Result#match_table.main_userId, U1),
    ?_assertEqual(Result#match_table.sec_userId, U2),
    ?_assertEqual(Result#match_table.requestId, RequestId)].
    
    
set_winner({RequestId, U1, U2})->
    Result = hd(usercom:new_match(RequestId, U1, U2)),
    usercom:set_winner(Result#match_table.id, U1),
    Result1 = hd(usercom:get_match(RequestId)),
    ?_assertEqual(Result1#match_table.winner, U1).
    
%%Test gps
gps_save({MatchId, U1, _U2})->
    usercom:gps_save(U1, MatchId, 2345678.4567890, 5678.5678),
   {_, _, _, Result, _} = database:db_query(gps_table_lookup,
		      "SELECT id FROM tGps WHERE matchId = ?",
		      [MatchId]),
    [?_assertEqual(length(hd(Result)), 1)].
   


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

create_users()->
    ok = account:register("test_usercom1","oaisdnni123asd","test_usercom1@mail.com"),
    ok = account:register("test_usercom2","os09mlsni123asd","test_usercom2@mail.com"),
    {ok, U1} = account:login("test_usercom1", "oaisdnni123asd"),
    {ok, U2} = account:login("test_usercom2", "os09mlsni123asd"),
    {U1, U2}.
 
delete_users({U1, U2})->
    account:delete(U1),
    account:delete(U2).
