%%Header file for database modules
%%
%%


%%@doc Records to hold database collected data.
-record(login_table, {id, salt, password}).
-record(loginlog_table, {id}).
-record(register_table,{id}).

-record(request_table,{id, challenged_userId, user_name, date, state, distance}).
-record(request_info_table, {requestId}).

-record(match_table,{id,userId, winner, requestId}).
-record(user_stats_table,{userName, averageSpeed, averageDistance, wins, matches, requests}).
-record(gps_table,{longitude, latitude, time}).
-record(user_search_table,{id, userName}).

-record(match_stats_table,{userId, time, winner, distance, averageSpeed, state}).

-type match_stats_table() ::{integer(), integer(), integer(), integer(), float(), integer()}.
-type login_table() :: {integer(), string(), string()}.
-type loginlog_table() :: {integer()}.
-type register_table() :: {integer()}.

-type request_table() :: {integer(), string(), integer(), integer(), integer()}.
-type request_info_table() :: {integer(), integer()}.

-type match_table() :: {integer(), integer(), integer(), integer(), integer()}.
-type user_stats_table() :: {string(), float(), float(), integer(), integer(), integer()}.
-type gps_table() :: {float(), float()}.
-type user_search_table() :: {integer(), string()}.
