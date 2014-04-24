
%% listener_sup

-module(listener_sup).
-behaviour(supervisor).

-export([start_link/2, start_listener/0, start_listeners/1, count_children/0]).
-export([init/1]).


%% Supervisor API

start_link(Port, Listeners) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Port, Listeners}).

start_listener() ->
    supervisor:start_child(?MODULE, []).

count_children() ->
    supervisor:count_children(?MODULE).


%% Callback functions

init({Port, Listeners}) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, inet, {active, once}, {packet, 4}]), 
    SuperSpec = {simple_one_for_one, 60, 3600},
    ChildSpec = {listener, 
		 {listener, start_link, [ListenSocket]}, 
		 temporary, 1000, worker, [listener]},
    spawn_link(?MODULE, start_listeners, [Listeners]),
    {ok, {SuperSpec, [ChildSpec]}}.


%% Help functions

start_listeners(Listeners) ->
    [start_listener() || _ <- lists:seq(1, Listeners)],
    ok.


