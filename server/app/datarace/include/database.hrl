%%Header file for database modules
%%
%%


%%@doc Records to hold database collected data.
-record(login_table, {id :: integer(), salt :: string(), password :: string()}).
-record(loginlog_table, {id :: integer()}).
-record(register_table,{id :: integer()}).
-record(request_table,{id, user_name, time, state}).



-type login_table() :: {integer(), string(), string()}.
-type loginlog_table() :: {integer()}.
-type register_table() :: {integer()}.
-type request_table() :: {integer(), string(), integer(), integer()}.
