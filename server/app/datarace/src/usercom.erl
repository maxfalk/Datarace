%%@doc Author: Max Falk Nilsson
%%This module holds functions for 
%%making requests and saving challange data.

-module(usercom).

-export([request/3,request_lookup/1,request_accept/1,request_cancel/1]).
-export([match/1,set_winner/2,match_stop/3]).
-export([gps_save/4, gps_get/2]).
-export([get_home_stats/1,get_num_pending_requests/1]).
-export([get_history/1, get_match_end_stats/1]).

-include("../include/database.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         REQUEST            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Make a new request to a user. Sending a request from you to
%% ChUserID with the challenge distance of Distance.
-spec request(UserId, ChUserId, Distance) -> ok when
      UserId :: integer(),
      ChUserId :: integer(),
      Distance :: integer().

request(UserId, ChUserList, Distance) when is_list(ChUserList) ->
    make_request(UserId, Distance),
    {ok, RequestId} = get_user_last_request(UserId),
    add_user_to_request(UserId,RequestId, 1),
    [add_user_to_request(UsrId, RequestId, 0) || UsrId <- ChUserList],
    ok;
request(UserId, ChUserId, Distance)->
    request(UserId,[ChUserId], Distance).


%%@doc Create the main request in the database.
-spec make_request(UserId, Distance)-> ok when
      UserId :: integer(),
      Distance :: integer().

make_request(UserId, Distance)->
    database:db_query(request_insert, 
		   <<"INSERT INTO tRequest (userId, distance, state, time)
                   VALUES(?, ?, 0, now())">>,
		  [UserId, Distance]).
    

%%@doc Get last request made by the user, from the database.
-spec get_user_last_request(UserId)-> {ok, integer()} | {error, undef} when
      UserId :: integer().

get_user_last_request(UserId)->
     Result = database:db_query(request_user_last, 
					  <<"SELECT max(t1.id) as id 
                                             FROM tRequest t1 WHERE t1.userId = ?">>,
					  [UserId]),
    case database:get_row(database:as_list(Result),1) of
	[{<<"id">>, undefined}] ->
	    {error, undef};
	[{<<"id">>, Value}] ->
	   {ok, Value}
    end.

    
    

%%@doc Add a user to the main request, this will connect the user to
%% the the main request.
-spec add_user_to_request(UserId, RequestId, State)-> ok when
      UserId :: integer(),
      RequestId :: integer(),
      State :: integer().

add_user_to_request(UserId, RequestId, State)->
    database:async_db_query(request_part_insert, 
		      <<"INSERT INTO tRequestedUsers (userId, requestId, state)
                         VALUES(?, ?, ?)">>,
		      [UserId, RequestId, State]).
      



%%@doc Lookup a users made requests and requests made against the user. 
%% Gives the result in a tuple with requests made first and requests made against the
%% user second.
-spec request_lookup(UserId) -> {[request_table(), ...], [request_table(), ...]} when
      UserId :: integer().

request_lookup(UserId)->
    {request_lookup_made(UserId), request_lookup_challenged(UserId)}.

%%@doc Lookup users requests made by userId. Including requests that are 
%% active and pending.
-spec request_lookup_made(UserId) -> [request_table(), ...] when
      UserId :: integer().


request_lookup_made(UserId) ->
    Sql_result = database:db_query(request_select_made, 
				   <<"SELECT t1.id, t4.id as challenged_userId, 
                                             t4.userName as user_name, 
                                             t2.time as date, t3.state, t2.distance
                                       FROM                               
                                       tRequestedUsers t1 inner join
                                       tRequest t2 on t1.requestId = t2.id and 
                                                t2.userId = t1.userId inner join
                                       tRequestedUsers t3 on t2.id = t3.requestId and 
                                                t2.userId != t3.userId inner join
                                       tUsers t4 on t3.userId = t4.id left join
                                       tMatchParticipant t5 on t5.requestedUserId = t1.id
                                      WHERE
                                       t1.userId = ? and
                                       t3.state != 2 and
                                       t5.id is null">>,
				   [UserId]),
    database:result_to_record(Sql_result, request_table).

%%@doc Lookup requests that have been made against Userid. Including active and pending 
%% requests.
-spec request_lookup_challenged(UserId)-> [request_table(), ...] when
      UserId :: integer().

request_lookup_challenged(UserId)->
    Sql_result = database:db_query(request_select_challanged, 
				   <<"SELECT t1.id, t3.id as challenged_userId, 
                                             t3.userName as user_name, 
                                             t2.time as date, t1.state, t2.distance
                                     FROM
                                      tRequestedUsers t1 inner join
                                      tRequest t2 on t1.requestId = t2.id and 
                                                     t2.userId != t1.userId inner join
                                      tUsers t3 on t2.userId = t3.id left join
                                      tMatchParticipant t4 on t4.requestedUserId = t1.id
                                      WHERE 
                                       t1.userId = ? and
                                       t1.state != 2 and
                                       t4.id is null;">>,
				   [UserId]),
    database:result_to_record(Sql_result, request_table).
    


%%@doc Accept a request with a given requestid.
-spec request_accept(UserRequestId)-> ok when
      UserRequestId :: integer().

request_accept(UserRequestId)->
    database:async_db_query(request_update_accept,
		   <<"UPDATE tRequestedUsers SET state = 1 WHERE id = ?">>,
		   [UserRequestId]).

%%@doc Cancel a request.
-spec request_cancel(UserRequestId)-> ok when
      UserRequestId :: integer().

request_cancel(UserRequestId)->
    database:async_db_query(request_update_cancel,
		   <<"UPDATE tRequestedUsers SET state = 2 WHERE id = ?">>,
		   [UserRequestId]).
    


%%@doc Get information about a request.
-spec get_request_info(UserRequestId)-> request_table() when
      UserRequestId :: integer().


get_request_info(UserRequestId)->
    SqlResult = database:db_query(request_get_users,
				   <<"SELECT requestId
                                      FROM
                                       tRequestedUsers t1
                                      WHERE
                                        t1.id = ?">>,
				   [UserRequestId]),
    database:get_row(database:result_to_record(SqlResult,request_info_table),1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         MATCH              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%@doc Create a new match for the given requestid if there isn't already a match
%% made for the request id. If the match is already created that match will be returned.
-spec match(UserRequestId)-> match_table() when
      UserRequestId :: integer().

match(UserRequestId)->
    Data = get_request_info(UserRequestId),
    Matchdata = get_match(Data#request_info_table.requestId, UserRequestId),
    case Matchdata of
	{error, no_item} ->
	    create_match(Data#request_info_table.requestId),
	    NewMatch = get_match(Data#request_info_table.requestId, UserRequestId),
	    add_user_to_match(UserRequestId, NewMatch#match_table.id),
	    get_match(Data#request_info_table.requestId, UserRequestId);
	_ ->
	    add_user_to_match(UserRequestId, Matchdata#match_table.id),
	    Matchdata
    end.


%%doc Create a new match.
-spec create_match(RequestId)-> ok when
      RequestId :: integer().

create_match(RequestId)->
    database:db_query(match_insert,
		   <<"INSERT INTO tMatch (winnerUserId, requestId, time, state)
                   VALUES(0, ?, now(), 0)">>,
		   [RequestId]).
    
%%@doc Get match information from requestid.
-spec get_match(RequestId, UserRequestId)-> match_table() when
      RequestId :: integer(),
      UserRequestId :: integer().

get_match(RequestId, UserRequestId)->
    Sql_result = database:db_query(match_select,
		   <<"select t1.id, t4.userId, t1.winnerUserId as winner, t3.id as requestId
                      from
                       tMatch t1 inner join
                       tRequestedUsers t3 on t1.requestId = t3.requestId inner join
                       tRequestedUsers t4 on t1.requestId = t4.requestId and t3.userId != t4.userId
                      where
                       t1.requestId = ? and
                       t3.id = ?
                      LIMIT 1;">>,
		     [RequestId, UserRequestId]),
    database:get_row(database:result_to_record(Sql_result, match_table),1).
   
    

%%@doc Add a user to a created match.
-spec add_user_to_match(UserRequestId, MatchId)-> ok when
      UserRequestId :: integer(),
      MatchId :: integer().

add_user_to_match(UserRequestId, MatchId)->
    case check_user_match(UserRequestId, MatchId) of
	false ->
	    database:async_db_query(insert_match_part,
				    "INSERT INTO 
                                     tMatchParticipant(requestedUserId, date, matchId, state)
                                     VALUES( ?, now(), ?, 0)",
				    [UserRequestId, MatchId]);
	true ->
	    ok
    end.


%%@doc Check if a user already is present in the match.
-spec check_user_match(UserRequestId, MatchId)-> boolean() when
      UserRequestId :: integer(),
      MatchId :: integer().

check_user_match(UserRequestId, MatchId)->
   Result =  database:db_query(select_match_par,
			      << "SELECT id FROM tMatchParticipant 
                                WHERE requestedUserId = ? and matchid = ?">>,
			       [UserRequestId, MatchId]),
    length(database:as_list(Result)) > 0.




%%@doc Stop the match and take actions to set the match as done, point out a winner if both
%% user as completed the race. save information about the race.
-spec match_stop(UserId, MatchId, UserRequestId)-> ok when
      UserId :: integer(),
      MatchId :: integer(),
      UserRequestId :: integer().
						   

match_stop(UserId, MatchId, UserRequestId)->
    user_match_stop(UserId, MatchId, UserRequestId),
    MatchDetails = get_match_details(MatchId),
    match_stophelper(MatchId, MatchDetails).


%%@doc Helper function to match stop, Does the calculation if a user is the winner or not.
-spec match_stophelper(MatchId, MatchDetails)-> ok when
      MatchId :: integer(),
      MatchDetails :: [[{binary(), integer()}, ...], ...].
						   

match_stophelper(MatchId, MatchDetails) when length(MatchDetails) > 1 ->        	
    case check_winner(MatchDetails) of
	WinnerId when WinnerId > 0 ->
	    set_winner(MatchId, WinnerId);
	_ -> no_winner
    end,
    set_match_done(MatchId);
match_stophelper(_MatchId, _MatchDetails)  ->
    ok.


%%@doc Make match stop actions for each user.
-spec user_match_stop(UserId, MatchId, UserRequestId)-> ok when
      UserId :: integer(),
      MatchId :: integer(),
      UserRequestId :: integer().

user_match_stop(UserId, MatchId, UserRequestId)->
    RunedDistance = gps:calc_totaldistance(UserId, MatchId),
    Distance = get_distance(UserRequestId),
    if
	RunedDistance >= Distance ->
	    log_serv:log("UID: " ++ integer_to_list(UserId) ++ "MID: " ++ integer_to_list(MatchId) ++ "finished the race"),
	    set_match_participant_done(UserRequestId, MatchId),
	    set_match_time(UserId, MatchId, UserRequestId);
	RunedDistance < Distance ->
	    log_serv:log("UID: " ++ integer_to_list(UserId) ++ "MID: " ++ integer_to_list(MatchId) ++ "forefeited the race"),
	    set_match_participant_forfeit(UserRequestId, MatchId),
	    set_match_time(UserId, MatchId, UserRequestId)
    end.


%%@doc Get the distance of a request.
-spec get_distance(UserRequestId)-> integer() when
      UserRequestId :: integer().

get_distance(UserRequestId)->
    Sql_result = database:db_query(select_distance,
				  <<"SELECT t1.distance
                                     FROM
                                      tRequest t1 inner join
                                      tRequestedUsers t2 on t1.id = t2.requestId
                                     WHERE
                                      t2.id = ?">>,
				   [UserRequestId]),
    [[{<<"distance">>, Distance}]] = database:as_list(Sql_result),
    Distance.

%%@doc Get details about a match.
-spec get_match_details(MatchId)-> [[{binary(), integer()}, ...], ...] when
      MatchId :: integer().

get_match_details(MatchId)->
    Sql_result = database:db_query(select_match_is_done,
				  <<"SELECT t3.userId, t2.state, t2.time 
                                     FROM tMatch t1 inner join
                                        tMatchParticipant t2 on t1.id = t2.matchId 
                                                          and t2.state != 0 inner join
                                        tRequestedUsers t3 on t1.requestId = t3.requestId
                                                              and t2.requestedUserId = t3.id
                                     WHERE
                                       t1.id = ?">>,
				   [MatchId]),
    database:as_list(Sql_result).



%%@doc Check which user won the match.
-spec check_winner(MatchDetails)-> integer() when
      MatchDetails :: [[{binary(), integer()}, ...], ...].

check_winner([[{<<"userId">>, UserId}, _,{<<"time">>, Time}] | T] = List) when length(List) > 1->
    check_winnerhelp(T, {UserId, Time});
check_winner(List)->
    0.

%%@doc Check winner help function does the actual calculation.
-spec check_winnerhelp(DetailsList, Result)-> {WinnerId, WinnerTime} when
      DetailsList :: [[{binary(), integer()}, ...], ...],
      Result :: {WinnerId, WinnerTime},
      WinnerId :: integer(),
      WinnerTime :: integer().

check_winnerhelp([], {WinnerId, _})->
    WinnerId;
check_winnerhelp([[{<<"userId">>, UserId}, {<<"state">>, 1},{<<"time">>, Time}] | T], {_WinnerId, WinnerTime}) when Time < WinnerTime ->
    check_winnerhelp(T,{UserId, Time});
check_winnerhelp([[{<<"userId">>, _UserId},  {<<"state">>, 2},{<<"time">>, Time}] | T], {WinnerId, WinnerTime}) when Time == WinnerTime, WinnerId == 0 -> 
    check_winnerhelp(T, {-1, WinnerTime});
check_winnerhelp([_|T], Result) ->
    check_winnerhelp(T, Result).

    
    
%%@doc Set match participant time. Set the time it took for the user to complete the race.
-spec set_match_time(UserId, MatchId, UserRequestId)-> ok when
      UserId :: integer(),
      MatchId :: integer(),
      UserRequestId :: integer().

set_match_time(UserId, MatchId, UserRequestId)->
    Total_time = gps:total_time(UserId, MatchId),
    database:db_query(insert_matchParticiapnt_time,
		     <<"UPDATE tMatchParticipant SET time = ?
                        WHERE matchId = ? and requestedUserId = ?">>,
		     [Total_time, MatchId, UserRequestId]).
    

%%@doc Set a user as the winner of a match.
-spec set_winner(MatchId, WinnerId)-> ok when
      MatchId :: integer(),
      WinnerId :: integer().

set_winner(MatchId, WinnerId)->
    database:async_db_query(match_select,
		      <<"UPDATE tMatch SET winnerUserId = ? WHERE id = ?">>,
		      [WinnerId, MatchId]),
    ok.
    
%%@doc Get the userid of the winner of a match.
-spec get_winner(MatchId)-> integer() when
      MatchId :: integer().

get_winner(MatchId)->
    Sql_result = database:db_query(get_match_winner,
				   <<"SELECT winnerUserId FROM tMatch WHERE id = ?">>,
				   [MatchId]),
    [[{<<"winnerUserId">>, Id}]] = database:as_list(Sql_result),
    Id.
    

%%@doc Set flag in the db that the user is finished running her turn in a match.
-spec set_match_participant_done(UserRequestId, MatchId)-> ok when
      UserRequestId :: integer(),
      MatchId :: integer().

set_match_participant_done(UserRequestId, MatchId)->
    database:db_query(set_match_participant_done,
			   <<"UPDATE tMatchParticipant SET state = 1 
                            WHERE requestedUserId = ? and matchId = ?">>,
			   [UserRequestId, MatchId]).


%%@doc Set flag in the db that the user is finished running her turn, and didn't complete
%% the total distance.
-spec set_match_participant_forfeit(UserRequestId, MatchId)-> ok when
      UserRequestId :: integer(),
      MatchId :: integer().

set_match_participant_forfeit(UserRequestId, MatchId)->
    database:db_query(set_match_participant_forefeit,
			   <<"UPDATE tMatchParticipant SET state = 2 
                            WHERE requestedUserId = ? and matchId = ?">>,
			   [UserRequestId, MatchId]).


%%@doc Set a flag that the match is completed, in the database.
-spec set_match_done(MatchId)-> ok when
      MatchId :: integer().

set_match_done(MatchId)->
    database:async_db_query(set_match_done,
			   <<"UPDATE tMatch SET state = 1 
                            WHERE id = ?">>,
			   [MatchId]).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         GPS                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%@doc Save gps information to the db, longitude and latitude, 
%%for a user in a specific match.
-spec gps_save(User_id, Match_id, Longidtude, Latitude) -> ok when
      User_id :: integer(), 
      Match_id :: integer(), 
      Longidtude :: integer(), 
      Latitude :: integer().
      


gps_save(User_id, Match_id, Longidtude, Latitude)->
    database:async_db_query(gps_insert,
		   <<"INSERT INTO tGps (matchParticipant, longitude, latitude, time)
                      SELECT t2.id, ?, ?, now()
                      FROM
                        tMatch t1 inner join
                        tMatchParticipant t2 on t1.id = t2.matchId inner join
                        tRequestedUsers t3 on t2.requestedUserId = t3.id
                      WHERE
                        t3.userId = ? and
                        t1.id = ?">>,
		   [Longidtude, Latitude, User_id, Match_id]).
    
%%@doc Get gps coordinates for a user in a specific match.
-spec gps_get(User_id, Match_id) -> [gps_table(), ...] when
      User_id :: integer(),
      Match_id :: integer().

gps_get(User_id,Match_id)->
    Sql_result = database:db_query(gps_get,
				   <<"SELECT t1.longitude, t1.latitude, t1.time
                                      FROM
                                        tGps t1 inner join
                                        tMatchParticipant t2 on t1.matchParticipant = t2.id
                                        inner join
                                        tRequestedUsers t3 on t2.requestedUserId = t3.id
                                      WHERE
                                       t3.userId = ? and
                                       t2.matchId = ?">>,
				   [User_id, Match_id]),
    database:result_to_record(Sql_result, gps_table).
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         STATISTICS         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Get the statistics that will be displayed in the home screen,
%%for a specific user.
-spec get_home_stats(UserId)-> user_stats_table() when
      UserId :: integer().

get_home_stats(UserId)->
    Sql_result = database:db_query(get_user_stats,
				   <<"SELECT userName, averageSpeed, averageDistance, 
                                             wins, matches, requests
                                      FROM
                                          tUserStatistics
                                      WHERE
                                          userId = ?">>,
				   [UserId]),
    pad_home_stats(
      database:get_row(database:result_to_record(Sql_result, user_stats_table),1)).
   

%%@doc Pad home_stats if it's empty with zero data.
-spec pad_home_stats(Value)-> user_stats_table() when
      Value :: user_stats_table() | {error, no_item}.

pad_home_stats({error, no_item})->
    #user_stats_table{userName = <<"">>, averageSpeed = 0, averageDistance = 0, wins = 0, 
		      matches = 0, requests = 0};
pad_home_stats(R) -> 
    R.



%%@doc Get number of pending requests for a user.
-spec get_num_pending_requests(UserId)-> integer() when
      UserId :: integer().

get_num_pending_requests(UserId)->
    {_, _, _, R, _} = database:db_query(get_user_pending_requests,
				   "SELECT count(t1.id) as pendingRequests
                                    FROM
                                      tRequestedUsers t1 
                                    WHERE
                                      t1.userId = ? and
                                      t1.state = 0",
				   [UserId]),
    pad_num_pending_requests(database:get_row(R, 1)).


%%@doc Pad get_num_pending_requests to always return a number.
-spec pad_num_pending_requests(R)-> integer() when
      R :: {error, no_item} | [integer()].

pad_num_pending_requests({error, no_item})->
    0;
pad_num_pending_requests(R) -> hd(R).



%%@doc Get match statisticsfor a user. Statistis for each match, include if it was a win
%% the distance, averageSpeed.
-spec get_history(UserId) -> [match_stats_table(), ...] when
      UserId :: integer().

get_history(UserId)->
    [Sql_result, _] = database:db_query(history_all_table,
				   <<"CALL all_match_stats(?)">>,
				   [UserId]),
    database:result_to_record(Sql_result, match_stats_table).


%%@doc Get match statistics, for a specific match.
-spec get_match_end_stats(MatchId)-> [match_stats_table(), ...] when
      MatchId :: integer().

get_match_end_stats(MatchId)->
    [Sql_result, _] = database:db_query(history_table,
				   <<"CALL match_stats(?)">>,
				   [MatchId]),
    database:result_to_record(Sql_result, match_stats_table).
    
