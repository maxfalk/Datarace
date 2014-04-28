%%
%%
%%
%%
%%

-module(log).

-behaviour(application).

-export([start/2,stop/1]).

start(_Type, _Args)->
    log_sup:start_link().


stop(_)->
    ok.
