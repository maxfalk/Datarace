%%@doc Author: Max Falk Nilsson
%%This module holds functions for 
%%making requests and saving challenge data.
%%!!!!!! Not finished !!!!!!!!

-module(usercom).

-export([request/3,request_lookup/1,request_accept/1,request_cancel/1]).
-export([create_match/3, get_match/1, new_match/3, set_winner/2]).
-export([gps_save/4]).
-export([get_home_stats/1,get_num_pending_requests/1]).

-include("../include/database.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         REQUEST            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Make a new request to a user
-spec request(UserId, Ch_userId, Distance) -> ok when
      UserId :: integer(),
      Ch_userId :: integer(),
      Distance :: integer().

request(UserId, Ch_userId, Distance)->
    database:db_query(request_insert, 
		   <<"INSERT INTO tRequest (userId, challenged_userId, distance, state, time)
                   VALUES(?, ?, ?, 0, now())">>,
		  [UserId, Ch_userId, Distance]),
    ok.

%%@doc Lookup users requests active and inactive.
-spec request_lookup(UserId) -> request_table() when
      UserId :: integer().


request_lookup(UserId) ->
    Sql_result = database:db_query(request_select, 
				   <<"SELECT t1.id, t1.challenged_userId, t2.user_name, 
                                      t1.time, t1.state FROM 
                                      tRequest t1 inner join tUsers t2 on 
                                      t1.challenged_userId = t2.id WHERE t1.userId = ?;">>,
		   [UserId]),
    database:result_to_record(Sql_result, request_table).


%%@doc Accept a request with a given request id.
-spec request_accept(Request_id)-> ok when
      Request_id :: integer().

request_accept(Request_id)->
    database:db_query(request_update_accept,
		   <<"UPDATE tRequest SET state = 1 WHERE id = ?">>,
		   [Request_id]),
    ok.

%%@doc Cancel a request.
-spec request_cancel(Request_id)-> ok when
      Request_id :: integer().

request_cancel(Request_id)->
    database:db_query(request_update_cancel,
		   <<"UPDATE tRequest SET state = 2 WHERE id = ?">>,
		   [Request_id]),
    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         MATCH              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%@doc Create a new match and return the created match details
-spec new_match(Request_id, Main_user, Sec_user)-> match_table() when
      Request_id :: integer(),
      Main_user :: integer(),
      Sec_user :: integer().

new_match(Request_id, Main_user, Sec_user)->
    create_match(Request_id, Main_user, Sec_user),
    get_match(Request_id).


%%doc Create a new match
-spec create_match(Request_id, Main_user, Sec_user)-> ok when
      Request_id :: integer(),
      Main_user :: integer(),
      Sec_user :: integer().

create_match(Request_id, Main_user, Sec_user)->
    database:db_query(match_insert,
		   <<"INSERT INTO tMatch (main_userId, sec_userId, winner, requestId)
                   VALUES(?, ?, ?, ?)">>,
		   [Main_user, Sec_user, 0, Request_id]),
    ok.
    
%%@doc Get match from request id
-spec get_match(Request_id :: integer())-> match_table().

get_match(Request_id)->
    Sql_result = database:db_query(match_select,
		   <<"SELECT id, main_userId, sec_userId, winner, requestId
                      FROM tMatch WHERE requestId = ?">>,
		   [Request_id]),
    database:result_to_record(Sql_result, match_table).
    

%%@doc set match winner
-spec set_winner(MatchId, WinnerId)-> ok when
      MatchId :: integer(),
      WinnerId :: integer().

set_winner(MatchId, WinnerId)->
    database:db_query(match_select,
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
    database:db_query(gps_insert,
		   <<"INSERT INTO tGps (userId, matchId, longitude, latitude, time)
                   VALUES(?, ?, ?, ?, now())">>,
		   [User_id, Match_id, Longidtude, Latitude]),
    ok.
    
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


%%@doc pad request to to always return a number
%%
-spec pad_num_pending_requests(R)-> integer() when
      R :: {error, no_item} | [integer()].

pad_num_pending_requests({error, no_item})->
    0;
pad_num_pending_requests(R) -> hd(R).
