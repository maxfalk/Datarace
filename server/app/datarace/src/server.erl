
%%@doc module for starting and stopping the servers applications

-module(server).

-export([start/1, stop/0]).


%%@doc start the server
-spec start(Log) -> ok when
      Log :: true | false.

start(Log) ->
    application:start(crypto),
    application:start(emysql),
    case Log of 
	true ->
	    application:start(log);
	false ->
	    ok
    end,
    application:start(datarace),
    application:start(search).


%%@doc Stop the server
-spec stop()-> ok.

stop() ->
    application:stop(datarace),
    application:stop(search),
    application:stop(log),
    application:stop(emysql),
    application:stop(crypto).
