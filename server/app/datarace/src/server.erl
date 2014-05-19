
%%@doc module for starting and stopping the servers applications
%%
%%

-module(server).

-export([start/0, stop/0]).

%%@doc start the server
%%
-spec start()-> ok.

start()->
    application:start(crypto),
    application:start(emysql),
    application:start(log),
    application:start(datarace).


%%@doc Stop the server
-spec stop()-> ok.

stop()->
    case whereis(master_sup) of
	undefined -> 
	    ok;
	_ ->
	    master_sup:stop_children()
    end,
    application:stop(datarace),
    application:stop(log),
    application:stop(emysql),
    application:stop(crypto).


    
