%%@doc Author: Max Falk Nilsson
%% This module conatins fucntions to start and stop
%% the database connection.
%%
%%

-module(database).

-export([init/0,stop/0]).

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
query(Binary_string)->
	emysql:execute(database_pool, Binary_string).


query(Name, Binary_string, Args)->
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


%%@doc
%%
%%
result_to_record(Sql_result, Record)->
        emysql:as_record(Sql_result, Record, record_info(fields, Record)).
