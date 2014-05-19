%%@doc Application for search server
%%
%%
%%

-module(search).
-export([start/2, stop/1]).

-behaviour(application).

%%@doc start the search application
-spec start(_Type :: atom(), _Args :: any()) -> {ok, pid()} 
						    | ignore
						    | {error, atom()}.

start(_Type, _Args)->
    search_sup:start_link().


%%@doc stop the search application
-spec stop(_Reason :: atom()) -> ok.

stop(_)->
    ok.
