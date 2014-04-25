
%%====================================================================
%% Client_serv_sup
%%====================================================================

-module(client_serv_sup).
-behaviour(supervisor).

-export([start_link/0, start_client_serv/2, count_children/0]).
-export([init/1]).


%%====================================================================
%% Supervisor API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client_serv(UserId, Socket) ->
    ChildSpec = {UserId, 
		 {client_serv, start_link, [UserId, Socket]}, 
		 temporary, 1000, worker, [client_serv]},
    supervisor:start_child(?MODULE, ChildSpec).

count_children() ->
    supervisor:count_children(?MODULE).


%%====================================================================
%% Callback functions
%%====================================================================

init(_Args) ->
    SuperSpec = {one_for_one, 60, 3600},
    {ok, {SuperSpec, []}}.

