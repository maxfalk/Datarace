
%%====================================================================
%% listener_sup
%%====================================================================


-module(listener_sup).
-behaviour(supervisor).

-export([start_link/2, start_listener/0, start_listeners/1, count_children/0]).
-export([init/1]).

-type socket() :: none().


%%====================================================================
%% Supervisor API
%%====================================================================


%% @doc Starts a new Listener supervisor, which in turn starts a 
%% number of Listeners and opens a port for incoming server 
%% connections.
-spec start_link(Port, Listeners) -> Result when
      Port :: integer(),
      Listeners :: integer(),
      Result :: {ok, pid()} | 
		ignore | 
		{error, Error},
      Error :: {already_started, pid()} | 
	       term().

start_link(Port, Listeners) when Listeners > 0 ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Port, Listeners}).


%% @doc Start a new Listener process dynamically.
-spec start_listener() -> Result when
      Result :: {ok, pid()}
	      | {error, Error},
      Error :: term().

start_listener() ->
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


%% @doc Initializes the listener supervisor by opening the port Port
%% and starts Listeners number of listener processes. 
-spec init({Port, Listeners}) -> {ok, {SuperSpec, [ChildSpec]}} when
      Port :: integer(),
      Listeners :: integer(),
      ListenSocket :: socket(),
      SuperSpec :: {simple_one_for_one, 100, 500},
      ChildSpec :: {listener, 
		    {listener, start_link, [ListenSocket]}, 
		    transient, 5000, worker, [listener]}.

init({Port, Listeners}) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, 
					       inet, 
					       {reuseaddr, true}, 
					       {active, once}, 
					       {packet, 4}]), 
    SuperSpec = {simple_one_for_one, 100, 500},
    ChildSpec = {listener, 
		 {listener, start_link, [ListenSocket]}, 
		 transient, 5000, worker, [listener]},
    spawn_link(?MODULE, start_listeners, [Listeners]),
    {ok, {SuperSpec, [ChildSpec]}}.


%%====================================================================
%% Help functions
%%====================================================================

%% @doc Function to help the supervisor start Listeners number of 
%% listeners during init.
-spec start_listeners(Listeners) -> ok when
      Listeners :: integer().

start_listeners(Listeners) ->
    [start_listener() || _ <- lists:seq(1, Listeners)],
    ok.


