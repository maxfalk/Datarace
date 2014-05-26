%%Tests for the account module. 
%%
%%
%%
-module(packconv_tests).

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
    RequestTable = {[{request_table, 1, 2, <<"hej">>, {datetime, {{3, 4, 5}, {6, 7, 8}}}, 9, 0}],
		    [{request_table, 1, 2, <<"hej">>, {datetime, {{3, 4, 5}, {6, 7, 8}}}, 9, 0}]},
    UserStatsTable = {user_stats_table, <<"hej">>, 12345.12345, 12345.12345, 1, 2, 3},
    SearchResults = [{user_search_table, 0, <<"aaa">>},
		     {user_search_table, 1, <<"bbb">>}],
    RLM = <<?REQUEST_LOOKUP_REPLY_MADE/binary, 1:32/little-integer, 2:32/little-integer, 
	    <<"hej">>/binary, 0:(8*47), 
	    3:32/little-integer, 4:32/little-integer, 5:32/little-integer, 6:32/little-integer, 
	    7:32/little-integer, 8:32/little-integer, 9:32/little-integer, 0:32/little-integer>>,
    RLC = <<?REQUEST_LOOKUP_REPLY_CHAL/binary, 1:32/little-integer, 2:32/little-integer, 
	    <<"hej">>/binary, 0:(8*47), 
	    3:32/little-integer, 4:32/little-integer, 5:32/little-integer, 6:32/little-integer, 
	    7:32/little-integer, 8:32/little-integer, 9:32/little-integer, 0:32/little-integer>>,
    UST = <<?GET_HOME_STATS_REPLY/binary, <<"hej">>/binary, 0:(47*8), 
	    12345.12345/little-float, 12345.12345/little-float, 
	    1:32/little-integer, 2:32/little-integer, 3:32/little-integer>>,
    SR = <<?SEARCH_RESULTS/binary, 0:32/little-integer, <<"aaa">>/binary, 0:(47*8),
	   1:32/little-integer, <<"bbb">>/binary, 0:(47*8)>>,
    [?_assertEqual({RLM, RLC}, packconv:pack(?REQUEST_LOOKUP, RequestTable)),
     ?_assertEqual(UST, packconv:pack(?GET_HOME_STATS, UserStatsTable)),
     ?_assertEqual(SR, packconv:pack(?SEARCH_RESULTS, SearchResults))].
