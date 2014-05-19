-module(matchlogic).
-export([winner/2]).
%=====================================%
%%announces the winner of the race    %
%=====================================%
winner(T1,T2) ->
    if T1 =:= T2 -> tie;
       T1 >= T2-> t1winner;
       T1 =< T2 -> t2winner end. 
		 
       
    
%========================================================%
%%compares the average speed from the other competitor
%% and tells if your race needs to speed up.%
%========================================================%
comparedistance(T1,T2)->
    if T1 =:= T2 -> evenrace;
       T1 >= T2 -> t2speedup; 
       T1 =< T2 -> t1speedup end. 
           
	    


