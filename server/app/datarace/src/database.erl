%%@doc Author: Max Falk Nilsson
%% This module conatins functions to start and stop
%% the database connection.
%%
%%

-module(database).

%%@doc result of a database query.
-type db_result() :: any().
-type db_start() :: any().

-export([init/0,stop/0, remote_connect/0]).
-export([db_query/3,db_query/1,get_row/2,result_to_record/2]).

-include("../include/database.hrl").

%%@doc Make a local connection to the database.
-spec init()-> db_start().

init()->
    emysql:add_pool(database_pool,[{size,1},
				   {user,"logger"},
				   {password,"squats1991"},
				   {database,"datarace"},
				   {encoding,utf8}]).


%%@doc Remote connect to the database
-spec remote_connect()-> db_start().

remote_connect()->
    emysql:add_pool(database_pool,[{size,1},
				   {host,"83.253.15.24"},
				   {port, 9099},
				   {user,"logger"},
				   {password,"squats1991"},
				   {database,"datarace"},
				   {encoding,utf8}]).

%%@doc Disconnect from the database.
-spec stop()-> ok.

stop()->
    emysql:remove_pool(database_pool).

%%@doc query the database, with a simple string.
-spec db_query(Binary_string)-> db_result() when
      Binary_string :: string().

db_query(Binary_string)->
	emysql:execute(database_pool, Binary_string).

%%@doc Query the database with a string, were "?" will be replaced
%% with the arguments in the list args in the same order as they are
%% placed in the string.
-spec db_query(Name, Binary_string, Args)-> db_result() when
      Name :: atom(),
      Binary_string :: string(),
      Args :: list().

db_query(Name, Binary_string, Args)->
	emysql:prepare(Name, Binary_string),
	emysql:execute(database_pool, Name, Args).


%%@doc Gets the nth row of data from a list.
-spec get_row(List, Num) -> any() | {error,no_item} when
      List :: list(),
      Num :: integer().


get_row(List, Num) when length(List) > 0->
    lists:nth(Num,List);
get_row(_List, _Num) ->
    {error, no_item}.

%%@doc Get the given result in a record specifed in the Record variable,
%% as one of the following login_table, loginlog_table or register_table.
-spec result_to_record(Sql_result, Record) -> record() when
      Sql_result :: db_result(),
      Record :: atom().

result_to_record(Sql_result, Record)->
    case Record of
	login_table ->
	    emysql:as_record(Sql_result, login_table, record_info(fields, login_table));
	loginlog_table ->
	   emysql:as_record(Sql_result, loginlog_table, record_info(fields, loginlog_table));
	register_table ->
	   emysql:as_record(Sql_result, register_table, record_info(fields, register_table));
	request_table ->
	    emysql:as_record(Sql_result, request_table, record_info(fields, request_table));
	match_table ->
	    emysql:as_record(Sql_result, match_table, record_info(fields, match_table));
	user_stats_table ->
        emysql:as_record(Sql_result, user_stats_table, record_info(fields, user_stats_table))
    end.
	        
