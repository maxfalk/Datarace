%%Tests for the account module. 
%%
%%
%%
-module(packconv_test).

-include ("../include/types.hrl").
-include_lib ("eunit/include/eunit.hrl").


%%====================================================================
%% Test description
%%====================================================================

packconv_test_()->
    {"Test packet conversion functionality: login, register",
      {setup, 
       fun start/0, 
       fun stop/1, 
       fun (SetupData) ->
	       [convert_pack_test(SetupData),
		pack_test(SetupData)]
       end}}.


%%====================================================================
%% Setup functions
%%====================================================================

start() ->
    ok.

stop(_) ->
    ok.
    

%%====================================================================
%% Actual tests
%%====================================================================

convert_pack_test(_SetupData) ->
    Login = "hej" ++ [ 0 || _ <- lists:seq(1, 47) ] ++ 
	"du" ++ [ 0 || _ <- lists:seq(1, 47) ],
    Register = "hej" ++ [ 0 || _ <- lists:seq(1, 47) ] ++ 
	"du" ++ [ 0 || _ <- lists:seq(1, 48) ] ++
	"glade" ++ [ 0 || _ <- lists:seq(1, 45) ],
    GPS = <<12345.12345/little-float, 99999.99999/little-float>>,
    SearchString = "gagagoogoo" ++ [ 0 || _ <- lists:seq(1, 40) ],
    [?_assertEqual({"hej", "du"},
		   packconv:convert_pack(?LOGIN, list_to_binary(Login))),
     ?_assertEqual({"hej", "du", "glade"},
		   packconv:convert_pack(?REGISTER, list_to_binary(Register))),
     ?_assertEqual({12345.12345, 99999.99999}, packconv:convert_pack(?MATCH_GPS, GPS)),
     ?_assertEqual("gagagoogoo", 
		   packconv:convert_pack(?SEARCH_STRING, list_to_binary(SearchString)))].


pack_test(_SetupData) ->
    RequestTable = {[{request_table, 1, 2, "hej", {datetime, {{3, 4, 5}, {6, 7, 8}}}, 9, 0}],
		    [{request_table, 1, 2, "hej", {datetime, {{3, 4, 5}, {6, 7, 8}}}, 9, 0}]},
    UserStatsTable = {user_stats_table, "hej", 12345.12345, 12345.12345, 1, 2, 3},
    SearchResults = [{user_search_table, 0, "aaa"},
		     {user_search_table, 1, "bbb"}],
    SR = <<5,1,0,0,0,0,"aaa",0,0,0,0,0,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	   0,0,1,0,0,0,"bbb",0,0,0,0,0,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    [?_assertEqual(something, packconv:pack(?REQUEST_LOOKUP, RequestTable)),
     ?_assertEqual(something, packconv:pack(?GET_HOME_STATS, UserStatsTable)),
     ?_assertEqual(SR, packconv:pack(?SEARCH_RESULTS, SearchResults))].
