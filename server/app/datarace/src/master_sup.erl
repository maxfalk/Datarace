
%%====================================================================
%% master_sup
%%====================================================================


-module(master_sup).
-behaviour(supervisor).

-export([start_link/0, stop_children/0]).
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


%% @doc Kill the Listener and Client Serv supervisors, in that order.
-spec stop_children() -> Result when
      Result :: ok | {error, Error},
      Error :: not_found | simple_one_for_one.

stop_children() ->
    case whereis(listener_sup) of
	undefined -> 
	    ok;
	_ ->
	    supervisor:terminate_child(?MODULE, listener_sup)
    end,
    case whereis(client_serv_sup) of
	undefined -> 
	    ok;
	_ ->
	    supervisor:terminate_child(?MODULE, client_serv_sup)
    end.


%%====================================================================
%% Callback functions
%%====================================================================


%% @doc Initializes the Master supervisor by first starting a 
%% Client_serv supervisor and then a Listener supervisor.

init(_Args) ->
    {Port, Listeners} = load_config(filename:absname("../configs/config")),
    SuperSpec = {rest_for_one, 60, 3600},
    ClientServSuperSpec = {client_serv_sup, 
			   {client_serv_sup, start_link, []}, 
			   permanent, 10000, supervisor, [client_serv_sup]},
    ListenerSuperSpec = {listener_sup, 
		       {listener_sup, start_link, [Port, Listeners]}, 
		       transient, 10000, supervisor, [listener_sup]},
    {ok, {SuperSpec, [ClientServSuperSpec, ListenerSuperSpec]}}.


%%====================================================================
%% Helper functions
%%====================================================================


%% @doc Load the port and listeners values from a config file.
-spec load_config(File) -> Result when
      File :: list(),
      Port :: integer() | false,
      Listeners :: integer() | false,
      Result :: {Port, Listeners}.

load_config(File) ->
    {ok, Config} = file:consult(File),
    {port, Port} = lists:keyfind(port, 1, Config),
    {listeners, Listeners} = lists:keyfind(listeners, 1, Config),
    {Port, Listeners}.
