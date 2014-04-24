%%@doc Author: Max Falk Nilsson
%%This module holds functions for 
%%making requests and saving challenge data.
%%!!!!!! Not finished !!!!!!!!

-module(usercom).

-export([request/3,request_lookup/1]).

%% Records for selecting from tables
-record(request_table,{id, user_name, time, state}).

-type request_table() :: {integer(), string(), integer(), integer()}.

%%@doc Make a new request to a user
-spec request(UserId, Ch_userId, Distance) -> ok when
      UserId :: integer(),
      Ch_userId :: integer(),
      Distance :: integer().

request(UserId, Ch_userId, Distance)->
    database:query(request_insert, 
		   <<"INSERT INTO tRequest (userId, challenged_userId, distance, state) 
                   VALUES(?, ?, ?, ?, 0)">>,
		  [UserId, Ch_userId, Distance]),
    ok.

%%@doc lookup users requests
-spec request_lookup(UserId) -> request_table() when
      UserId :: integer().


request_lookup(UserId) ->
    Sql_result = database:query(request_select, 
		   <<"SELECT id, user_name, time, state FROM tRequest t1 inner join 
                   tUsers t2 on t1.challnged_userId = t2.id WHERE t1.userId = ?;">>,
		   [UserId]),
    database:result_to_record(Sql_result,request_table).


%%@doc accept a request
-spec request_accept(Request_id)-> ok when
      Request_id :: integer().

request_accept(Request_id)->
    database:query(request_update_accept,
		   <<"UPDATE tRequest SET state = 1 WHERE id = ?">>,
		   [Request_id]),
    ok.

%%@doc Cancel a reuqest
-spec request_cancel(Request_id)-> ok when
      Request_id :: integer().

request_cancel(Request_id)->
    database:query(request_update_cancel,
		   <<"UPDATE tRequest SET state = 2 WHERE id = ?">>,
		   [Request_id]),
    ok.
    

%%@doc Save gps information to the db.
-spec gps_save(User_id, Match_id, Longidtude, Latitude, Time) -> ok when
      User_id :: integer(), 
      Match_id :: integer(), 
      Longidtude :: integer(), 
      Latitude :: integer(), 
      Time :: integer().


gps_save(User_id, Match_id, Longidtude, Latitude, Time)->
    database:query(gps_insert,
		   <<"INSERT INTO tGps (userId, matchId, longitude, latitude, time)
                   VALUES(?, ?, ?, ?, ?)">>,
		   [User_id, Match_id, Longidtude, Latitude, Time]),
    ok.
    
