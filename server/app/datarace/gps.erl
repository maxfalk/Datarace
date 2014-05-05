%%@doc Author: Marina Jaksic
%% This module contains functions for calculating the distance with the points for longitude and latitude. 
%% We can aswell get the speed of the race, average speed and average distance
%% from this module.  
-module(gps).

-export([distance/4, speed/2, averagespeed/2, averagedistance/2]).




%%@doc===============================================================================%
%% calculates the distance between two points and gives us the distance in km.  
%====================================================================================%

-spec distance(Long1, Lat1, Long2, Lat2) -> ok when
	Long1 :: interger(),
	Lat1 :: interget(),
	Long2 :: interget(),
	Lat2 :: interger ().

distance(Long1, Lat1, Long2, Lat2) ->
    DegToRad = fun(Deg) -> math:pi()*Deg/180 end,
    [RLong1, RLat1, RLong2, RLat2] = [DegToRad(Deg) || Deg <- [Long1, Lat1, Long2, Lat2]],
 
    DLong = RLong2 - RLong1,
    DLat = RLat2 - RLat1,
 
    A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLong/2), 2),
 
    C = 2 * math:asin(math:sqrt(A)),
 
    %% radius of Earth is 6372.8 km
    Km = 6372.8 * C,
    Km.


%%@doc==================================================%
%%calculates the speed from given time and distance
%=======================================================%

-spec speed(Time, Distance) -> ok when
	Time :: interger(),
	Distance :: interger().

speed(Time, Distance) ->
    Distance/Time.  
%%@doc=============================================%
%%calculates the average speed                
%==================================================%

-spec averagespeed(Totaldistance, Totaltime)-> ok when
	Totaldistance :: interger(),
	Totaltime :: interger(). 

averagespeed(Totaldistance, Totaltime) ->
    Totaldistance/Totaltime. 
%%@doc============================================%    
%%calculates the average distance            
%=================================================%

-spec averagedistance(Totaltime, Totalspeed) -> ok when 

	Totaltime :: interger(),
	Totalspeed :: interger(). 

averagedistance(Totaltime, Totalspeed) ->
   Totaltime*Totalspeed.

