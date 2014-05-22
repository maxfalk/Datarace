%%
%%Search supervisor
%%

-module(search_sup).

-export([start_link/0, start_search_serv/0, count_children/0]).
-export([init/1]).

-behaviour(supervisor).

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


start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).




%% @doc Start a new child process dynamically. 

-spec start_search_serv() -> Result when
      Result :: {ok, pid()}
	      | {error, Error},
      Error :: term().

start_search_serv() ->
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc init callback function to start a new child with a 
%%simple on for one restart plan.
-spec init(Args) -> any() when
      Args :: none().

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 60},
          [{search_serv, {search_serv, start_link, []},
	    temporary, 2000, worker, [search_serv]}]}}.

