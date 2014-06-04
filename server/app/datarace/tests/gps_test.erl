%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testcases.                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gps_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/database.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       TEST DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gps_test_()->
    {"Test gps functions",
    {setup, fun setup/0, fun stop/1,
     fun (SetupData)->
	     [distance_test(SetupData),
	     speed_test(SetupData),
	     averagespeed_test(SetupData),
	     averagedistance_test(SetupData),
	     calc_pointdistance_test(SetupData),
	     total_time_test(SetupData),
	     calc_totaldistance_test(SetupData)]
    end}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       SETUP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup()->
    {U1, U2} = create_users(),
    usercom:request(U1, U2, 1),
    timer:sleep(100),

    %% Player 1
    {R_U1, _} = usercom:request_lookup(U1),
    Result_U1 = hd(R_U1),
    timer:sleep(100),
    Result2_U1 = usercom:match(Result_U1#request_table.id),
    timer:sleep(100),
    Time_U1 = calendar:local_time(),
    usercom:gps_save(U1, Result2_U1#match_table.id, 2345678.4567890, 5678.5678),
    timer:sleep(3000),
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
    timer:sleep(3000),
    usercom:gps_save(U2, Result2_U2#match_table.id, 2345678.4567890, 6678.5678),
    timer:sleep(100),
    usercom:match_stop(U2, Result2_U2#match_table.id, Result_U2#request_table.id),
    {U1, U2, Result2_U2#match_table.id, Time_U1}.





stop({U1, U2,  _, _})->
    delete_users({U1, U2}).    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   ACTUALL TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

distance_test(_)->
    [?_assert(gps:distance(500359, 583838, 0054253, 0030412) =:= 11253.768718183694),
    ?_assert(gps:distance(0,0,0,0) =:= 0.0),
    ?_assert(gps:distance(2,3,0,0) =:= 400.975880277179)].

speed_test(_)->
    [?_assert(gps:speed(2,400) =:= 200.0),
    ?_assert(gps:speed(10,10) =:= 1.0),
    ?_assert(gps:speed(100,100) =:= 1.0),
    ?_assert(gps:speed(67,83) =:= 1.2388059701492538)].

averagespeed_test(_)->
    [?_assert(gps:averagespeed(60,2) =:= 30.0),
    ?_assert(gps:averagespeed(368,8) =:= 46.0),
    ?_assert(gps:averagespeed(1,1) =:= 1.0)].   

averagedistance_test(_)->
    [?_assertEqual(gps:averagedistance(120,60), 7200),
    ?_assertEqual(gps:averagedistance(1,1),  1)].



calc_pointdistance_test({U1, _, Matchid, Time})->
    [?_assert(gps:calc_pointdistance(U1, Matchid, Time) > 120),
     ?_assertEqual(gps:calc_pointdistance(-1, -1, calendar:local_time()), 0.0)].


calc_totaldistance_test({U1, _, Matchid, _})->
    [?_assert(gps:calc_totaldistance(U1, Matchid) > 120)].


total_time_test({U1, _, Matchid, _})->
    [?_assert(gps:total_time(U1, Matchid) > 2.9)].




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
    account:delete("test_usercom44"),
    account:delete("test_usercom45"),
    U1 = create_user("test_usercom44","oaisdnni123asd"),
    timer:sleep(100),
    U2 = create_user("test_usercom45", "os09mlsni123asd"),
    {U1, U2}.
 
delete_users({U1, U2})->
    account:delete(U1),
    account:delete(U2).
