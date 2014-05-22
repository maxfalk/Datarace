%%@doc Author: Max Falk Nilsson
%%This module holds functions for 
%%making requests and saving challange data.
%%!!!!!! Not finished !!!!!!!!

-module(usercom).

-export([request/3,request_lookup/1,request_accept/1,request_cancel/1]).
-export([match/1, set_winner/2]).
-export([gps_save/4, gps_get/2]).
-export([get_home_stats/1,get_num_pending_requests/1]).

-include("../include/database.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         REQUEST            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Make a new request to a user
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


%%@doc make main reuqest.
-spec make_request(UserId, Distance)-> ok when
      UserId :: integer(),
      Distance :: integer().

make_request(UserId, Distance)->
    database:db_query(request_insert, 
		   <<"INSERT INTO tRequest (userId, distance, state, time)
                   VALUES(?, ?, 0, now())">>,
		  [UserId, Distance]).
    

%%@doc get last request made by the user
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

    
    

%%@doc add users to requests
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
%% active, inactive and pending.
-spec request_lookup_made(UserId) -> [request_table(), ...] when
      UserId :: integer().


request_lookup_made(UserId) ->
    Sql_result = database:db_query(request_select_made, 
				   <<"SELECT t1.id, t2.id as challenged_userId, 
                                             t3.userName as user_name, 
                                             t2.time, t1.state, t2.distance
                                      FROM
                                       tRequestedUsers t1 inner join
                                       tRequest t2 on t1.requestId = t2.id and 
                                                t2.userId = t1.userId inner join
                                       tUsers t3 on t2.userId != t3.id
                                      WHERE
                                       t1.userId = ?">>,
				   [UserId]),
    database:result_to_record(Sql_result, request_table).

%%@doc Lookup requests that have been made against Userid. Including active, inactive
%% and pending.
-spec request_lookup_challenged(UserId)-> [request_table(), ...] when
      UserId :: integer().

request_lookup_challenged(UserId)->
    Sql_result = database:db_query(request_select_challanged, 
				   <<"SELECT t1.id, t2.id as challenged_userId, 
                                             t3.userName as user_name, 
                                             t2.time, t1.state, t2.distance
                                     FROM
                                      tRequestedUsers t1 inner join
                                      tRequest t2 on t1.requestId = t2.id and 
                                                     t2.userId != t1.userId inner join
                                      tUsers t3 on t2.userId = t3.id
                                      WHERE 
                                       t1.userId = ?;">>,
				   [UserId]),
    database:result_to_record(Sql_result, request_table).
    


%%@doc Accept a request with a given request id.
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


%%@doc Create a new match for the given request id if there isn't already a match
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
	    add_user_to_match(UserRequestId, NewMatch#match_table.id);

	_ ->
	    add_user_to_match(UserRequestId, Matchdata#match_table.id),
	    Matchdata
    end.


%%doc Create a new match
-spec create_match(RequestId)-> ok when
      RequestId :: integer().

create_match(RequestId)->
    database:db_query(match_insert,
		   <<"INSERT INTO tMatch (winnerUserId, requestId, time, state)
                   VALUES(0, ?, now(), 0)">>,
		   [RequestId]).
    
%%@doc Get match from request id
-spec get_match(RequestId, UserRequestId)-> match_table() when
      RequestId :: integer(),
      UserRequestId :: integer().

get_match(RequestId, UserRequestId)->
    Sql_result = database:db_query(match_select,
		   <<"select t1.id, t3.userId, t1.winnerUserId as winner, t1.requestId
                      from
                       tMatch t1 inner join
                       tRequestedUsers t3 on t1.requestId = t3.requestId
                      where
                       t1.requestId = ? and
                       t3.id != ?">>,
		     [RequestId, UserRequestId]),
    database:get_row(database:result_to_record(Sql_result, match_table),1).
   
    

%%@doc add user to match

add_user_to_match(UserRequestId, MatchId)->
    database:db_query(insert_match_part,
		      "INSERT INTO tMatchParticipant(requestedUserId, time, matchId, state)
                       VALUES(?, now(), ?, 0)",
		     [UserRequestId, MatchId]).
    

%%@doc set match winner
-spec set_winner(MatchId, WinnerId)-> ok when
      MatchId :: integer(),
      WinnerId :: integer().

set_winner(MatchId, WinnerId)->
    database:async_db_query(match_select,
		      <<"UPDATE tMatch SET winner = ? WHERE id = ?">>,
		      [WinnerId, MatchId]),
    ok.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         GPS                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%@doc Save gps information to the db.
-spec gps_save(User_id, Match_id, Longidtude, Latitude) -> ok when
      User_id :: integer(), 
      Match_id :: integer(), 
      Longidtude :: integer(), 
      Latitude :: integer().
      


gps_save(User_id, Match_id, Longidtude, Latitude)->
    database:async_db_query(gps_insert,
		   <<"INSERT INTO tGps (userId, matchId, longitude, latitude, time)
                   VALUES(?, ?, ?, ?, now())">>,
		   [User_id, Match_id, Longidtude, Latitude]),
    ok.
    
%%@doc Get gps coordinates for a user in a specific match.
-spec gps_get(User_id, Match_id) -> [gps_table(), ...] when
      User_id :: integer(),
      Match_id :: integer().

gps_get(User_id,Match_id)->
    Sql_result = database:db_query(gps_get,
				   <<"SELECT longitude, latitude, time
                                      FROM
                                        tGps
                                      WHERE
                                        userId = ? and
                                        matchId = ?">>,
				   [User_id, Match_id]),
    database:result_to_record(Sql_result, gps_table).
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         STATISTICS         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Get the statistics that will be displayed in the home screen,
%%for a specific user
%%
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
   

%%@doc pad home_stats if it's empty with zero data.
-spec pad_home_stats(Value)-> user_stats_table() when
      Value :: user_stats_table() | {error, no_item}.

pad_home_stats({error, no_item})->
    #user_stats_table{userName = <<"">>, averageSpeed = 0, averageDistance = 0, wins = 0, 
		      matches = 0, requests = 0};
pad_home_stats(R) -> 
    R.



%%@doc get number of pending requests for a user.
%%
%%
-spec get_num_pending_requests(UserId)-> integer() when
      UserId :: integer().

get_num_pending_requests(UserId)->
    {_, _, _, R, _} = database:db_query(get_user_pending_requests,
				   "SELECT count(t1.id) as pendingRequests
                                    FROM
                                      tRequest t1
                                    WHERE
                                      t1.userId = ? and
                                      t1.state = 0
                                   GROUP BY 
                                      t1.id",
				   [UserId]),
    pad_num_pending_requests(database:get_row(R, 1)).


%%@doc pad request to always return a number
%%
-spec pad_num_pending_requests(R)-> integer() when
      R :: {error, no_item} | [integer()].

pad_num_pending_requests({error, no_item})->
    0;
pad_num_pending_requests(R) -> hd(R).
