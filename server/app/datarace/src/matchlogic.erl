-module(matchlogic).
-export([winner/2,comparedistance/2]).
%=====================================%
%%announces the winner of the race    %
%=====================================%
-spec winner(T1,T2) -> tie | t1winner | t2winner when
      T1 :: integer(),
      T2 :: integer().

winner(T1,T2) ->
    if T1 =:= T2 -> tie;
       T1 >= T2-> t1winner;
       T1 =< T2 -> t2winner end. 
		 
       
    
%========================================================%
%%compares the average speed from the other competitor
%% and tells if your race needs to speed up.%
%========================================================%
-spec comparedistance(T1,T2) -> Result when
      T1 :: float(),
      T2 :: float(),
      Result :: t1_distance_zero | t2_distance_zero| evenrace | t2speedup| t1speedup.
    
comparedistance(0,_T2)-> t1_distance_zero;
comparedistance(_T1,0)-> t2_distance_zero;
comparedistance(T1,T2)->
    if T1 =:= T2 -> evenrace;
       T1 > T2 -> t2speedup; 
       T1 < T2 -> t1speedup end. 
           
	    


