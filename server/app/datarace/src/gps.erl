-module(gps).
-export([distance/4, speed/2, averagespeed/2, averagedistance/2, statistics_compare/3]).
-include_lib("../include/database.hrl").
%%@doc===============================================%
%% calculates the distance between two points   %
%===============================================%
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
%%calculates the speed from given time and distance%
%==================================================%
speed(Time, Distance) ->
    Distance/Time.  
%%@doc=============================================%
%%calculates the average speed                %   
%=============================================%
averagespeed(Totaldistance, Totaltime) ->
    Totaldistance/Totaltime. 
%%@doc============================================%    
%% calculates the average distance            %
%============================================%
averagedistance(Totaltime, Totalspeed) ->
   Totaltime*Totalspeed.

%%getgps i usercom
%%listan med gps kooridnaer ska till distance sen
calc_totaldistance(User_id1, Match_id) ->
   Gps1 =  usercom:gps_get(User_id1, Match_id), 
    calc_totaldistancehelp(Gps1, 0).
    
    
calc_totaldistancehelp([], Distance) ->
    Distance;

calc_totaldistancehelp([Last], Distance) ->
    Distance;
    
calc_totaldistancehelp([First,Sec | Tl], Distance) ->
  Sum_distance =  distance(First#gps_table.longitude, First#gps_table.latitude, Sec#gps_table.longitude, Sec#gps_table.latitude),
    
calc_totaldistancehelp([Sec|Tl], Sum_distance + Distance).




%%@doc==============================================%
%% get the gps coordinates and speed from the other %
%% competitor at a chosen time.                     %
%===================================================%
statistics_compare(User_id1, Match_id1, User_id2) ->
    Distance_User1 = calc_totaldistance(User_id1, Match_id1),
    Distance_User2 = calc_totaldistance(User_id2, Match_id1), 
    matchlogic:comparedistance(Distance_User1, Distance_User2),
    {matchlogic:comparedistance(Distance_User1, Distance_User2), Distance_User1 - Distance_User2}.




	
				       
