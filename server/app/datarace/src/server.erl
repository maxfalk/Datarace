

-module(server).

-export([start/0, stop/0]).

start()->
    application:start(crypto),
    application:start(emysql),
    application:start(datarace).


stop()->
    application:stop(datarace),
    application:stop(emysql),
    application:stop(crypto).


    
