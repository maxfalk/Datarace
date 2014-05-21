
%%@doc module for starting and stopping the servers applications
%%
%%

-module(server).

-export([start/1, stop/0]).

%%@doc start the server
%%
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
    application:start(datarace).


%%@doc Stop the server
-spec stop()-> ok.

stop() ->
    case whereis(master_sup) of
	undefined -> 
	    ok;
	_ ->
	    master_sup:stop_children()
    end,
    application:stop(datarace),
    case whereis(log_sup) of
	undefined -> 
	    ok;
	_ ->
	    application:stop(log)
    end,
    application:stop(emysql),
    application:stop(crypto).


    
