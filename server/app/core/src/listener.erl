
%% listener

-module(listener).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, connect/2, terminate/3, handle_info/3]).


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
	    listener_sup:start_listener(),
	    {next_state, login, AcceptSocket};
	{error, Reason} ->
	    listener_sup:start_listener(),
	    {stop, Reason, ListenSocket} 
    end.

handle_info({tcp, _Socket, Packet}, login, AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, once}]),
    io:format("Received message: ~s~n", [binary_to_list(Packet)]),
    gen_tcp:send(AcceptSocket, Packet),
    {next_state, login, AcceptSocket};
handle_info({tcp_closed, _Socket}, _State, AcceptSocket) ->
    gen_tcp:close(AcceptSocket),
    {stop, normal, AcceptSocket}.

terminate(_Reason, init, _ListenSocket) ->
    io:format("Terminating~n"),
    ok;
terminate(_Reason, connect, _ListenSocket) ->
    io:format("Terminating~n"),
    ok;
terminate(_Reason, _State, AcceptSocket) ->
    io:format("Terminating~n"),
    gen_tcp:close(AcceptSocket),
    ok.
