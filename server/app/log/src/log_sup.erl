%%@doc Supervisor for the logserver
%%
%%
%%
%%
-module(log_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [verbose]).


%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%

init(Type) -> 
    {ok, {{one_for_one, 5, 60},
          [{log_serv, {log_serv, start_link, Type},
	    permanent, 2000, worker, [log_serv]}]}}.
