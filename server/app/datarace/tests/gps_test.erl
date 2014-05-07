%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testcases.                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gps_test).


-include_lib("eunit/include/eunit.hrl").
%%testgps
%%Test that the result call will give the right value
distance_test()->
    ?assert(gps:distance(500359, 583838, 0054253, 0030412) =:= 11253.768718183694),
    ?assert(gps:distance(0,0,0,0) =:= 0.0),
    ?assert(gps:distance(2,3,0,0) =:= 400.975880277179).

speed_test()->
    ?assert(gps:speed(2,400) =:= 200.0),
    ?assert(gps:speed(10,10) =:= 1.0),
    ?assert(gps:speed(100,100) =:= 1.0),
    ?assert(gps:speed(67,83) =:= 1.2388059701492538).

averagespeed_test()->
    ?assert(gps:averagespeed(60,2) =:= 30.0),
    ?assert(gps:averagespeed(368,8) =:= 46.0),
    ?assert(gps:averagespeed(1,1) =:= 1.0).   

averagedistance_test()->
    ?assertEqual(gps:averagedistance(120,60), 7200),
    ?assertEqual(gps:averagedistance(1,1),  1).
