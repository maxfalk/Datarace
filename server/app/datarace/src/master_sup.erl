
%% master_sup

-module(master_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


%% Supervisor API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% Callback functions

init(_Args) ->
    Port = 8888,
    Listeners = 10,
    SuperSpec = {rest_for_one, 60, 3600},
    ClientServSuperSpec = {client_serv_sup, 
			   {client_serv_sup, start_link, []}, 
			   permanent, 10000, supervisor, [client_serv_sup]},
    ListenerSuperSpec = {listener_sup, 
		       {listener_sup, start_link, [Port, Listeners]}, 
		       permanent, 10000, supervisor, [listener_sup]},
    {ok, {SuperSpec, [ClientServSuperSpec, ListenerSuperSpec]}}.
