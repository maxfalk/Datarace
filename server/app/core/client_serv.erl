
%% client_serv

-module(client_serv).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2]).


%% Server API

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% Callback functions

init(_Args) ->
    {ok, []}.

handle_info({tcp, Socket, Data}, LoopData) ->
    io:format(Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, LoopData}.
