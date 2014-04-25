
%%====================================================================
%% Listener
%%====================================================================

-module(listener).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, connect/2, login/2, terminate/3, handle_info/3]).

-include("../include/types.hrl").


%%====================================================================
%% Server API
%%====================================================================

%% @doc Starts a new Listener process, waiting for incoming client
%% connections. Once a connection is established, it will wait for the
%% client to log in and spawn a new client_serv process to handle the
%% connection.

start_link(ListenSocket) ->
    gen_fsm:start_link(?MODULE, ListenSocket, []).


%%====================================================================
%% Callback functions
%%====================================================================


%% @doc Initializes the Listener and tells the gen_fsm behaviour to  
%% go to and execute the connect state directly.

init(ListenSocket) ->
    io:format("Initializing~n"),
    gen_fsm:send_event(self(), initialized),
    {ok, connect, ListenSocket}.


%% @doc Waits for incoming connections. Moves to the login state if 
%% a connection was successfully established. Stops the process otherwise.
 
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


%% @doc 

login({?LOGIN, Packet}, AcceptSocket) ->
    {UserName, Password} = packconv:convert_pack(?LOGIN, Packet),
    io:format("UN: ~p, PW: ~p~n", [UserName, Password]),
    case account:login(UserName, Password) of
	{ok, UserId} ->
	    gen_tcp:send(AcceptSocket, ?LOGIN_TRUE),
	    io:format("Logged in~n"),
	    {ok, Pid} = client_serv_sup:start_client_serv(), %% Send UserId and AcceptSocket
	    ok = gen_tcp:controlling_process(AcceptSocket, Pid), 
	    {stop, normal, AcceptSocket};
	{error, no_user} -> 
	    gen_tcp:send(AcceptSocket, ?LOGIN_FALSE_USERNAME),
	    io:format("Login in failed: Wrong username~n"),
	    {next_state, login, AcceptSocket};
	{error, wrong_password} ->
	    gen_tcp:send(AcceptSocket, ?LOGIN_FALSE_PASSWORD),
	    io:format("Login in failed: Wrong password~n"),
	    {next_state, login, AcceptSocket}
    end;
login({?REGISTER, Packet}, AcceptSocket) ->
    {UserName, Password, Email} = packconv:convert_pack(?REGISTER, Packet),
    io:format("UN: ~p, PW: ~p, EM: ~p~n", [UserName, Password, Email]),
    case account:register(UserName, Password, Email) of
	ok ->
	    io:format("User registered~n"),
	    gen_tcp:send(AcceptSocket, ?REGISTER_TRUE);
	{error, user_already_exist} ->
	    io:format("User not registered~n"),
	    gen_tcp:send(AcceptSocket, ?REGISTER_FALSE)
    end,		
    {next_state, login, AcceptSocket};
login(Packet, AcceptSocket) ->
    %% Received an unknown packet, log it and stay in the state
    io:format("Received weird message: ~w~n", [Packet]),
    {next_state, login, AcceptSocket}.


%% @doc Receives {tcp, Socket, Package} messages as events and forwards 
%% them to the correct state. Returns {stop, normal, Socket} if the message 
%% is {tcp_closed, Reason} or {tcp_error, Socket, Reason}, which will 
%% terminate the process and execute terminate/3.

handle_info({tcp, _, <<Type:1/binary, Packet/binary>>}, State, AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, once}]),
    ?MODULE:State({Type, Packet}, AcceptSocket);
handle_info({tcp, _t, <<Type:1/binary>>}, State, AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, once}]),
    ?MODULE:State(Type, AcceptSocket);
handle_info({tcp_closed, _}, _State, Socket) ->
    {stop, normal, Socket};
handle_info({tcp_error, _, _Reason}, _State, Socket) ->
    {stop, normal, Socket}.


%% @doc Executed when Listener is about to terminate. Socket is closed
%% if State is not connect.

terminate(_Reason, connect, _ListenSocket) ->
    io:format("Terminating~n");
terminate(_Reason, _State, AcceptSocket) ->
    gen_tcp:close(AcceptSocket),
    io:format("Terminating~n").
