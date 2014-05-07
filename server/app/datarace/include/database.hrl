%%Header file for database modules
%%
%%


%%@doc Records to hold database collected data.
-record(login_table, {id, salt, password}).
-record(loginlog_table, {id}).
-record(register_table,{id}).
-record(request_table,{id, challenged_userId, user_name, time, state}).
-record(match_table,{id, main_userId, sec_userId, winner, requestId}).



-type login_table() :: {integer(), string(), string()}.
-type loginlog_table() :: {integer()}.
-type register_table() :: {integer()}.
-type request_table() :: {integer(), string(), integer(), integer()}.
-type match_table() :: {integer(), integer(), integer(), integer(), integer()}.
