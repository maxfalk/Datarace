-module(gps).
-export([distance/4, speed/2, averagespeed/2, averagedistance/2, statistics_compare/3]).
-export([calc_pointdistance/3,calc_totaldistance/2, total_time/2]).
-compile(export_all).
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

%%@doc total gps time
total_time(UserId, MatchId)->
    total_timehelper(usercom:gps_get(UserId, MatchId), 0).

total_timehelper([], Time)->
    Time;
total_timehelper([_First], Time)->
    Time;
total_timehelper([First, Sec | T], Time)->
    total_timehelper(T, Time + calc_timediff(First#gps_table.time, Sec#gps_table.time)).


%%@doc calculate the difference in time of two date times.
-spec calc_timediff(T1,T2) -> integer() when
      T1 :: any(),
      T2 :: any().

calc_timediff({datetime, T1},{datetime, T2}) ->
    calc_timediff(T1,T2);
calc_timediff(T1,T2) ->
    Sec_time1= calendar:datetime_to_gregorian_seconds(T1),
    Sec_time2 = calendar:datetime_to_gregorian_seconds(T2),
    if
	Sec_time1 < Sec_time2 ->
	    Sec_time2 - Sec_time1;
	Sec_time1 > Sec_time2 ->
	    Sec_time1 - Sec_time2;
	Sec_time1 == Sec_time2 ->
	    0
    end.
    
%%@doc Calculates the distance at a given time point
-spec calc_pointdistance(UserId, MatchId, StartTime)-> float() when
      UserId :: integer(),
      MatchId :: integer(),
      StartTime :: any().

calc_pointdistance(UserId, MatchId, StartTime)->
    Gps = usercom:gps_get(UserId, MatchId),
    Time = calc_timediff(calendar:local_time(), StartTime),
    calc_pointdistancehelp(Gps, Time, 0, 0, UserId).


calc_pointdistancehelp([], Maxtime, Distance, Time, UserId) ->
    Distance + calc_avgdistance_from_avgspeed(UserId,Maxtime- Time);
calc_pointdistancehelp([_Last], Maxtime, Distance, Time, UserId) ->
        Distance + calc_avgdistance_from_avgspeed(UserId,Maxtime- Time);
calc_pointdistancehelp([First,Sec | Tl], Maxtime, Distance, Time, UserId) ->
    NewTime = Time + calc_timediff(First#gps_table.time, Sec#gps_table.time),
    if
	NewTime =< Maxtime ->
	    Sum_distance = distance(First#gps_table.longitude, First#gps_table.latitude, 
				    Sec#gps_table.longitude, Sec#gps_table.latitude),
	    calc_pointdistancehelp([Sec|Tl], Maxtime, Sum_distance + Distance, NewTime, UserId);
	 NewTime > Maxtime ->
	    Distance
    end.

%%@doc calculate distance for a given time using average speed of a user,
%% time is in seconds.
-spec calc_avgdistance_from_avgspeed(UserId, Time)-> float() when
      UserId :: integer(),
      Time :: integer().


calc_avgdistance_from_avgspeed(UserId, Time) when Time > 0->
    AvgSpeed = get_averagespeed(UserId),
    case AvgSpeed of
	0 ->
	    averagedistance(10, (Time/3600));
	_ ->
	    averagedistance(AvgSpeed, (Time/3600))
    end;
calc_avgdistance_from_avgspeed(_,_) ->
    0.

    


%%@doc Get averagespeed fomr the db
-spec get_averagespeed(UserId)-> float() when
      UserId :: integer().

get_averagespeed(UserId)->
    Result = database:db_query(average_speed_avgdistance,
		     "SELECT t1.averageSpeed 
                      FROM tUserStatistics t1
                      WHERE
                       t1.userId = ?",
		      [UserId]),
    case database:as_list(Result) of
	 [[{<<"averageSpeed">>, AverageSpeed}]] -> 
	    AverageSpeed;
	_ ->
	    0
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




	
				       
