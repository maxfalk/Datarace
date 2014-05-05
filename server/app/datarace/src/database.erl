%%@doc Author: Max Falk Nilsson
%% This module conatins fucntions to start and stop
%% the database connection.
%%
%%

-module(database).

-export([init/0,stop/0]).
-export([db_query/3,db_query/1,get_row/2,result_to_record/2]).

-include("../include/database.hrl").
%%@doc init login in to database
%%
%%
init()->
    emysql:add_pool(database_pool,[{size,1},
				  {user,"logger"},
				  {password,"squats1991"},
				  {database,"datarace"},
				  {encoding,utf8}]).

%%@doc stop the database
%%
%%
%%
stop()->
    emysql:remove_pool(database_pool).

%%@doc query the database
%%
%%
%%
db_query(Binary_string)->
	emysql:execute(database_pool, Binary_string).


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

%%@doc Get the given result in a record specifed in the Record variable.
%%
%%
-spec result_to_record(Sql_result, Record) -> record() when
      Sql_result :: string(),
      Record :: atom().

result_to_record(Sql_result, Record)->
    case Record of
	login_table ->
	    emysql:as_record(Sql_result, login_table, record_info(fields, login_table));
	loginlog_table ->
	   emysql:as_record(Sql_result, loginlog_table, record_info(fields, loginlog_table));
	register_table ->
	    emysql:as_record(Sql_result, register_table, record_info(fields, register_table))
    end.
	        
