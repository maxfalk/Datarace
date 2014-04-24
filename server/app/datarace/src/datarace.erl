%%Appliction for the server core
%%
%%
%%
%%

-module(datarace).

-behaviour(application).

-export([start/2,stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(normal, [])->
    database:init(),
    master_sup:start_link().


stop(_)->
    database:stop(),
    ok.
