-module(gps).
-export([distance/4, speed/2, averagespeed/2, averagedistance/2, statistics_compare/3]).
-export([calc_pointdistance/3]).
-include_lib("../include/database.hrl").

%%@doc===============================================%
%% calculates the distance between two points   %
%===============================================%
-spec distance(Long1, Lat1, Long2, Lat2) -> float() when
      Long1 :: float(),
      Lat1 :: float(),
      Long2 :: float(),
      Lat2 :: float().

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
-spec speed(Time, Distance) -> float() when
      Time :: integer(),
      Distance :: integer().

speed(Time, Distance) ->
    Distance/Time.  
%%@doc=============================================%
%%calculates the average speed                %   
%=============================================%
-spec averagespeed(Totaldistance, Totaltime) -> float() when
      Totaldistance :: float(),
      Totaltime :: float().

averagespeed(Totaldistance, Totaltime) ->
    Totaldistance/Totaltime. 
%%@doc============================================%    
%% calculates the average distance            %
%============================================%
-spec averagedistance(Totaltime, Totalspeed) -> integer() when
      Totaltime :: integer(),
      Totalspeed :: integer().

averagedistance(Totaltime, Totalspeed) ->
   Totaltime*Totalspeed.

%%@doc calculate the total distance for the user in a specific match.
-spec calc_totaldistance(User_id1, Match_id) -> float() when
      User_id1 :: integer(),
      Match_id :: integer().


calc_totaldistance(User_id1, Match_id) ->
    Gps1 = usercom:gps_get(User_id1, Match_id), 
    calc_totaldistancehelp(Gps1, 0).
    
calc_totaldistancehelp([], Distance) ->
    Distance;
calc_totaldistancehelp([_Last], Distance) ->
    Distance;
calc_totaldistancehelp([First,Sec | Tl], Distance) ->
    Sum_distance = distance(First#gps_table.longitude, First#gps_table.latitude, 
			     Sec#gps_table.longitude, Sec#gps_table.latitude),
    calc_totaldistancehelp([Sec|Tl], Sum_distance + Distance).




%%@doc calculate the difference in time of two date times.
-spec calc_timediff(T1,T2) -> integer() when
      T1 :: any(),
      T2 :: any().

calc_timediff(T1,T2) when T1 > T2 ->
    calendar:datetime_to_gregorian_seconds(T1) -
	calendar:datetime_to_gregorian_seconds(T2);
calc_timediff(T1,T2) when T1 < T2 ->
    calendar:datetime_to_gregorian_seconds(T1) -
	calendar:datetime_to_gregorian_seconds(T2);
calc_timediff(_T1,_T2) ->
    0.

%%@doc Calculates the distance at a given time point
-spec calc_pointdistance(UserId, MatchId, StartTime)-> float() when
      UserId :: integer(),
      MatchId :: integer(),
      StartTime :: any().

calc_pointdistance(UserId, MatchId, StartTime)->
    Gps = usercom:gps_get(UserId, MatchId),
    Time = calc_timediff(calendar:local_time(), StartTime),
    calc_pointdistancehelp(Gps, Time, 0, 0).


calc_pointdistancehelp([], _, Distance, _) ->
    Distance;
calc_pointdistancehelp([_Last], _, Distance, _) ->
    Distance;
calc_pointdistancehelp([First,Sec | Tl], Maxtime, Distance, Time) ->
    NewTime = Time + calc_timediff(First#gps_table.time, Sec#gps_table.time),
    if
	NewTime =< Maxtime ->
	    Sum_distance = distance(First#gps_table.longitude, First#gps_table.latitude, 
				    Sec#gps_table.longitude, Sec#gps_table.latitude),
	    calc_pointdistancehelp([Sec|Tl], Maxtime, Sum_distance + Distance, NewTime);
	 NewTime > Maxtime ->
	    Distance
    end.




    

%%@doc==============================================%
%% get the gps coordinates and speed from the other %
%% competitor at a chosen time.                     %
%===================================================%
-spec statistics_compare(User_id1, Match_id1, User_id2) -> Result when
      User_id1 :: integer(),
      Match_id1 :: integer(),
      User_id2 :: integer(),
      Result :: 
	{t1_distance_zero | t2_distance_zero| evenrace | t2speedup| t1speedup , float()}.

statistics_compare(User_id1, Match_id1, User_id2) ->
    Distance_User1 = calc_totaldistance(User_id1, Match_id1),
    Distance_User2 = calc_totaldistance(User_id2, Match_id1), 
    matchlogic:comparedistance(Distance_User1, Distance_User2),
    {matchlogic:comparedistance(Distance_User1, Distance_User2), Distance_User1 - Distance_User2}.




	
				       
