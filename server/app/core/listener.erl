
%% listener

-module(listener).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, connect/2, terminate/2, handle_info/3]).


%% Server API

start_link(ListenSocket) ->
    gen_fsm:start_link(?MODULE, ListenSocket, []).


%% Callback functions

init(ListenSocket) ->
    io:format("Initializing~n"),
    gen_fsm:send_event(self(), initialized),
    {ok, connect, ListenSocket}.

connect(initialized, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, AcceptSocket} ->
	    io:format("Connected~n"),
	    {next_state, login, AcceptSocket};
	{error, Reason} ->
	    io:format("Terminating~n"),
	    {stop, Reason, ListenSocket} 
    end.

handle_info({tcp, _Socket, Packet}, login, AcceptSocket) ->
    io:format("Received message~n"),
    gen_tcp:send(AcceptSocket, Packet),    
    {next_state, login, AcceptSocket}.

terminate(_Reason, _Socket) ->
    ok.
