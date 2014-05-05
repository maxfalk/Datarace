%%@doc Supervisor for the logserver
%%
%%
%%
%%
-module(log_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%@doc Start supervisor, in verbose mode.
%%
%%
-spec start_link()-> {ok, pid()}
			     | ignore
			     | {error, atom()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [verbose]).


%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Callback function for supervisor, specifices how the supervisor
%%should handle children.
%%
-spec init(Type :: [atom(), ...])-> tuple().


init(Type) -> 
    {ok, {{one_for_one, 5, 60},
          [{log_serv, {log_serv, start_link, Type},
	    permanent, 2000, worker, [log_serv]}]}}.
