%%@doc header file for database modules
%%
%%



%%@doc Records to hold database data
-record(user_search_table,{id, userName}).


-type user_search_table() :: {integer(), string()}.
