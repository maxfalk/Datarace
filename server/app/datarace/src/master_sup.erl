
%%====================================================================
%% master_sup
%%====================================================================

-module(master_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


%%====================================================================
%% Supervisor API
%%====================================================================

%% @doc Start a new Master supervisor. 

-spec start_link() -> Result when
      Result :: {ok, pid()} | 
		ignore | 
		{error, Error},
      Error :: {already_started, pid()} | 
	       {shutdown, term()} | 
	       term().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Callback functions
%%====================================================================

%% @doc Initializes the Master supervisor by first starting a 
%% Client_serv supervisor and the a Listener supervisor.

init(_Args) ->
    %%{Port, Listeners} = load_config(filename:absname("../configs/config")),
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


%%====================================================================
%% Helper functions
%%====================================================================

load_config(File) ->
    {ok, Config} = file:consult(File),
    {port, Port} = lists:keyfind(port, 1, Config),
    {listeners, Listeners} = lists:keyfind(listeners, 1, Config),
    {Port, Listeners}.
