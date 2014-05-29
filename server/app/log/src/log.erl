%%@doc Auther: Max Falk
%%Application interface for log server

-module(log).

-behaviour(application).

-export([start/2,stop/1]).

%%@doc Start the application.
-spec start(_Type :: atom(), _Args :: any()) -> {ok, pid()} 
							| ignore
							| {error, atom()}.

start(_Type, _Args)->
    log_sup:start_link().

%%@doc Stop the application
-spec stop(_Reason :: atom()) -> ok.

stop(_)->
    ok.
