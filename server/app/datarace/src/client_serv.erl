
%%====================================================================
%% Client_serv
%%====================================================================

-module(client_serv).
-behaviour(gen_fsm).

-export([start_link/2, verify_control_transfer/1]).
-export([init/1, verify_login/2, main/2, handle_info/3, terminate/3]).

-include("../include/types.hrl").


%%====================================================================
%% Server API
%%====================================================================

start_link(UserId, Socket) ->
    gen_server:start_link(?MODULE, {UserId, Socket}, []).

verify_control_transfer(Pid) ->
    gen_server:send_even(Pid, control_transferred).


%%====================================================================
%% Callback functions
%%====================================================================

init({UserId, Socket}) ->
    io:format("Spawned new child."),
    {ok, verify_login, {UserId, Socket}}.

verify_login(control_transferred, {UserId, Socket}) ->
    gen_tcp:send(Socket, ?LOGIN_TRUE),
    {next_state, main, {UserId, Socket}}.

main(?LOGIN_LOGOUT, {UserId, Socket}) ->
    ok = account:logout(UserId),
    io:format("Logged out: ~w~n", [UserId]),
    {stop, normal, {UserId, Socket}}.

handle_info({tcp, _, <<Packet/binary>>}, State, AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, once}]),
    ?MODULE:State(Packet, AcceptSocket);
handle_info({tcp_closed, _}, _State, Socket) ->
    io:format("Disconnected unexpectedly~n"),
    %% Connection was unexpectedly lost. Log this stuff.
    {stop, normal, Socket};
handle_info({tcp_error, _, _Reason}, _State, Socket) ->
    io:format("Unexpected TCP error~n"),
    %% A TPC error occurred. Log this stuff.
    {stop, normal, Socket}.

terminate(_Reason, _State, Socket) ->
    gen_tcp:close(Socket),
    io:format("Terminating in client_serv~n").
