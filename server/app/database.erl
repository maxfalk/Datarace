-module(database).

-export([])


%%@doc init login in to database
%%
%%
init()->
    cryto:start(), %% only needed for testing will be started seperatly in an real release
    application:start(emysql),
    emysql:add_pool(database_pool,[{size,1},
				  {user,"logger"},
				  {password,"squats1991"},
				  {database,"datarace"},
				  {encoding,utf8}]).

%%@doc Checks if the given user name and user password 
%% match the user in the database
%%
-spec login()

login(User_name,Password)->
    





















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%         TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
