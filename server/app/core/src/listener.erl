
%% listener

-module(listener).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, connect/2, login/2, terminate/3, handle_info/3]).

-define(LOGIN_INFO, <<0>>).
-define(LOGIN_REPLY, <<1>>).
-define(REGISTER_INFO, <<2>>).


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

login({?LOGIN_INFO, Packet}, AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, once}]),
    {UserName, Password} = packconv:convert_pack(0,Packet),
    io:format("UN: ~p, PW: ~p",[UserName,Password]),
    case account:login(UserName, Password) of
	{ok, UserId} -> 
	    gen_tcp:send(AcceptSocket, <<"You are logged in">>),
	    io:format("Logged in~n"),
	    {next_state, login, AcceptSocket};
	_Packet -> 
	    gen_tcp:send(AcceptSocket, <<"Login failed">>),
	    io:format("Login in failed~n"),
	    {next_state, login, AcceptSocket}
    end;
login(Packet, AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, once}]),
    io:format("Received weird message: ~w~n", [Packet]),
    {stop, normal, AcceptSocket}.

handle_info({tcp, _, <<Type:1/binary, Packet/binary>>}, State, Socket) ->
    ?MODULE:State({Type, Packet}, Socket);
handle_info({tcp, _t, <<Type:1/binary>>}, State, Socket) ->
    ?MODULE:State(Type, Socket);
handle_info({tcp_closed, _}, _State, Socket) ->
    {stop, normal, Socket};
handle_info({tcp_error, _, _Reason}, _State, Socket) ->
    {stop, normal, Socket}.

terminate(_Reason, _State, _Socket) ->
    io:format("Terminating~n").
