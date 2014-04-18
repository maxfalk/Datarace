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

