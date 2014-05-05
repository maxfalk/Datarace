%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testcases.                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(testgps).

%%testgps
%%Test that the result call will give the right value
distance_test()->
?assert(distance(500359, 583838, 0054253, 0030412) =:= 11253.768718183694).
?assert(distance(0,0,0,0) =:= 0.0).   
?assert(distance(2,3,0,0) =:= 400.975880277179).

speed_test()->
?assert(speed(2,400) =:= 200,0).
?assert(speed(10,10) =:= 1.0).
?assert(speed(100,100) =:= 1.0).
?assert(speed(67,83) =:= 1,2388059701492538).

averagespeed_test()->
?assert(averagespeed(60,2) =:= 30.0).
?assert(averagespeed(368,8) =:= 46.0).
?assert(averagespeed(1,1) =:= 1.0).   

averagedistande_test()->
?assert(averadedistance(120,60) =:= 7200).
?assert(averagedistance(1,1) =:= 1).
