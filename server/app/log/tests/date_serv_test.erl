%%Tests for the date_serv module.
%%
%%
%%
-module(data_serv).




%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test(_)->
	?assert(start())







init_test(_)->
    ?assert(database:init()).

stop_test(_)->
    ?assert(database:stop()).



distance_test()->
?assert(distance(500359, 583838, 0054253, 0030412) =:= 11253.768718183694).
?assert(distance(0,0,0,0) =:= 0.0).   
?assert(distance(2,3,0,0) =:= 400.975880277179).
