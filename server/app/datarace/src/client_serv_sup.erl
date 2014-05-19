
%%====================================================================
%% client_serv_sup
%%====================================================================

-module(client_serv_sup).
-behaviour(supervisor).

-export([start_link/0, start_client_serv/2, count_children/0]).
-export([init/1]).

-type socket() :: none().


%%====================================================================
%% Supervisor API
%%====================================================================

%% @doc Start a new Client_serv supervisor, which will wait for 
%% instructions to start new Client_servs. 

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

-spec start_client_serv(UserId, Socket) -> Result when
      UserId :: integer(),
      Socket :: socket(),
      Result :: {ok, pid()}
	      | {error, Error},
      Error :: term().

start_client_serv(UserId, Socket) ->
    ChildSpec = {UserId, 
		 {client_serv, start_link, [UserId, Socket]}, 
		 temporary, 1000, worker, [client_serv]},
    supervisor:start_child(?MODULE, ChildSpec).


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

%% @doc Initializes the Client_serv supervisor.

-spec init(Args) -> {ok, {SuperSpec, []}} when
      Args :: none(),
      SuperSpec :: {simple_one_for_one, 60, 3600}.

init(_Args) ->
    SuperSpec = {one_for_one, 60, 3600},
    {ok, {SuperSpec, []}}.

