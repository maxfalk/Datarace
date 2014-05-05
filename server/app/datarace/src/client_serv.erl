
%%====================================================================
%% Client_serv
%%====================================================================

-module(client_serv).
-behaviour(gen_fsm).

-export([start_link/2, verify_control_transfer/1]).
-export([init/1, verify/2, main/2, handle_info/3, terminate/3]).

-type socket() :: none().

-include("../include/types.hrl").


%%====================================================================
%% Server API
%%====================================================================

%% @doc Starts a new Client_serv process. 

-spec start_link(UserId, Socket) -> Result when
      UserId :: integer(),
      Socket :: socket(),
      Error :: term(),
      Result :: {ok, pid()} | ignore | {error, Error}.

start_link(UserId, Socket) ->
    gen_fsm:start_link(?MODULE, {UserId, Socket}, []).


%% @doc Sends an asynchronous message to the Client_serv with pid Pid
%% telling it that it has the contol over a socket and may use it. 
%% Only relevant just after initialization, when Client_serv is in its
%% verify state. 

-spec verify_control_transfer(Pid) -> ok when
      Pid :: pid().

verify_control_transfer(Pid) ->
    gen_fsm:send_event(Pid, control_transferred).


%%====================================================================
%% Callback functions
%%====================================================================

%% @doc Initialize the Client_serv by doing nothing.

-spec init(Args) -> {ok, verify, Args} when
      UserId :: integer(),
      Socket :: socket(),
      Args :: {UserId, Socket}.

init(Args) ->
    io:format("Spawned new client_serv.~n"),
    {ok, verify, Args}.


%% @doc Sends a LOGIN_TRUE message to the client when receiving a 
%% control_transferred event, informing the client about a successful 
%% login attempt.

-spec verify(control_transferred, {UserId, Socket}) -> Result when
      UserId :: integer(),
      Socket :: socket(),
      Result :: {next_state, main, {UserId, Socket}}.

verify(control_transferred, {UserId, Socket}) ->
    io:format("Control over socket transferred for UID: ~w~n", [UserId]),
    ok = gen_tcp:send(Socket, ?LOGIN_TRUE),
    {next_state, main, {UserId, Socket}}.


%% @doc Recieves a LOGIN_LOGOUT message from a client and logs out the
%% user from the database. Stops the FSM.

-spec main(LOGIN_LOGOUT, {UserId, Socket}) -> Result when
      LOGIN_LOGOUT :: binary(),
      UserId :: integer(),
      Socket :: integer(),
      Result :: {stop, normal, {UserId, Socket}}.

main(?LOGIN_LOGOUT, LoopData) ->
    {stop, normal, LoopData}.


%% @doc Receives TCP packets and forwards them to the function 
%% representing the current state or stops if the message is 
%% erroneous.

-spec handle_info(Event, State, LoopSocket) -> Result when
      Event :: {tcp, Socket, Packet} | 
	       {tcp_closed, Socket} | 
	       {tcp_error, Socket, Reason},
      State :: term(),
      LoopSocket :: socket(),
      Socket :: socket(),
      Packet :: binary(),
      Reason :: term(),
      Result :: none() | {stop, normal, LoopSocket}.

handle_info({tcp, _, <<Packet/binary>>}, State, {UserId, Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:State(Packet, {UserId, Socket});
handle_info({tcp_closed, _}, _State, {UserId, Socket}) ->
    io:format("Disconnected unexpectedly~n"),
    %% Connection was unexpectedly lost. Log this stuff.
    {stop, normal, {UserId, Socket}};
handle_info({tcp_error, _, _Reason}, _State, Socket) ->
    io:format("Unexpected TCP error~n"),
    %% A TPC error occurred. Log this stuff.
    {stop, normal, Socket}.


%% @doc The function is called upon termination and makes sure that
%% the Socket is closed.

-spec terminate(Reason, State, {UserId, Socket}) -> none() when
      Reason :: term(),
      State :: atom(),
      UserId :: integer(),
      Socket :: socket().

terminate(Reason, State, {UserId, _Socket}) ->
    ok = account:logout(UserId),
    io:format("Logged out: ~w~n", [UserId]),
    io:format("Terminating client_serv: ~w ~w~n", [State, Reason]).
