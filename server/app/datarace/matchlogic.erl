%%@doc Author: Marina Jaksic
%% This module announces the winner of the race.
%% It also compares the speed from the other competitor and let
%% you know if you need to improve you race.
-module(matchlogic).

-export([winner/2]).

%%@doc=====================================================================%
%%announces the winner of the race with the time as input from each player     
%===============================x==========================================%

-spec winner(T1, T2) -> ok when
	T1 :: interger(),
	t2 :: interger ().

winner(T1,T2) ->
    if T1 =:= T2 -> tie;
       T1 >= T2-> t1winner;
       T1 =< T2 -> t2winner end. 
		 
       
    
%%@doc=======================================================%
%%compares the average speed from the other competitor
%% and tells if your race needs to speed up.
%============================================================%

- spec comparespeed() -> ok when
	T1 :: interger,
	T2 :: interger. 

comparespeed(T1,T2)->
    if T1 =:= T2 -> evenrace;
       T1 >= T2 -> t2speedup; 
       T1 =< T2 -> t1speedup end. 
           
	    


