-module(gps).
-export([distance/4, speed/2, averagespeed/2, averagedistance/2]).


%% calculates the distance between two points
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



%%calculates the speed from given time and distance
speed(Time, Distance) ->
    Distance/Time.  

%%calculates the average speed    
averagespeed(Totaldistance, Totaltime) ->
    Totaldistance/Totaltime. 
    
%%calculates the average distance
averagedistance(Totaltime, Totalspeed) ->
   Totaltime*Totalspeed.

