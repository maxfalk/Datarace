
%%====================================================================
%% client_serv_sup
%%====================================================================


-module(client_serv_sup).
-behaviour(supervisor).

-export([start_link/0, start_client_serv/0, count_children/0]).
-export([init/1]).

-type socket() :: none().


%%====================================================================
%% Supervisor API
%%====================================================================


%% @doc Start a new Client Serv supervisor, which will wait for 
%% instructions to start new Client Servs. 
-spec start_link() -> Result when
      Result :: {ok, pid()} | 
		ignore | 
		{error, Error},
      Error :: {already_started, pid()} | 
	       {shutdown, term()} | 
	       term().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc Start a new client_serv dynamically. 
-spec start_client_serv() -> Result when
      Result :: {ok, pid()}
	      | {error, Error},
      Error :: term().

start_client_serv() ->
    supervisor:start_child(?MODULE, []).


%% @doc Returns information concerning the number of active processes.
-spec count_children() -> PropListOfCounts when
      PropListOfCounts :: [Count],
      Count :: {specs, integer()}
	     | {active, integer()}
	     | {supervisors, integer()}
	     | {workers, integer()}.

count_children() ->
    supervisor:count_children(?MODULE).


%%====================================================================
%% Callback functions
%%====================================================================


%% @doc Initializes the client_serv supervisor.
-spec init(Args) -> {ok, {SuperSpec, [ChildSpec]}} when
      Args :: none(),
      SuperSpec :: {simple_one_for_one, 60, 3600},
      ChildSpec :: {client_serv, 
		    {client_serv, start_link, []}, 
		    temporary, 5000, worker, [client_serv]}.

init(_Args) ->
    SuperSpec = {simple_one_for_one, 60, 3600},
    ChildSpec = {client_serv, 
		 {client_serv, start_link, []}, 
		 temporary, 5000, worker, [client_serv]},
    {ok, {SuperSpec, [ChildSpec]}}.

