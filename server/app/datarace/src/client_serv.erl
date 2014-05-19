
%%====================================================================
%% Client_serv
%%====================================================================

-module(client_serv).
-behaviour(gen_fsm).

-export([start_link/2, verify_control_transfer/1]).
-export([init/1, verify/2, main/2, match/2, handle_info/3, terminate/3]).

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

init({UserId, _Socket} = Args) ->
    process_flag(trap_exit, true),
    log_serv:log("Spawned new client_serv for UID " ++ integer_to_list(UserId)),
    {ok, verify, Args}.


%% @doc Sends a LOGIN_TRUE message to the client when receiving a 
%% control_transferred event, informing the client about a successful 
%% login attempt.

-spec verify(control_transferred, {UserId, Socket}) -> Result when
      UserId :: integer(),
      Socket :: socket(),
      Result :: {next_state, main, {UserId, Socket}}.

verify(control_transferred, {UserId, Socket}) ->
    log_serv:log("Socket transfer for UID " ++ integer_to_list(UserId)),
    ok = gen_tcp:send(Socket, ?LOGIN_TRUE),
    {next_state, main, {UserId, Socket}}.


%% @doc Handles request messages from a client and, when it recieves 
%% a logout message from a client, logs out the user from the 
%% database and stops the FSM.

-spec main(Message, {UserId, Socket}) -> Result when
      Type :: binary(),
      Package :: binary(),
      Message :: {Type, Package} | Type,
      UserId :: integer(),
      Socket :: integer(),
      Result :: {stop, normal, {UserId, Socket}}.

main(?LOGIN_LOGOUT, LoopData) ->
    {stop, normal, LoopData};
main({?REQUEST, <<ChallengeId:32/little-integer, Distance:32/little-integer>>}, {UserId, Socket}) ->
    log_serv:log("Match request made by UID " ++ 
		     integer_to_list(UserId) ++ " for " ++
		     integer_to_list(ChallengeId) ++ " distance " ++
		     integer_to_list(Distance)),
    usercom:request(UserId, ChallengeId, Distance),
    {next_state, main, {UserId, Socket}};
main(?REQUEST_LOOKUP, {UserId, Socket}) ->
    log_serv:log("Match request lookup made by UID " ++ 
		     integer_to_list(UserId)),
    RequestTableTuple = usercom:request_lookup(UserId),
    {MadePack, ChalPack} = packconv:pack(?REQUEST_LOOKUP, RequestTableTuple),
    log_serv:log("Number of made requests is " ++ 
		     integer_to_list((byte_size(MadePack)-2) div 90) ++
		     " number of received requests is " ++ 
		     integer_to_list((byte_size(ChalPack)-2) div 90) ++
		     " packet size " ++ integer_to_list(byte_size(MadePack)) ++ 
		     "/" ++ integer_to_list(byte_size(ChalPack))),
    ok = gen_tcp:send(Socket, MadePack),
    ok = gen_tcp:send(Socket, ChalPack),
    {next_state, main, {UserId, Socket}};
main({?REQUEST_ACCEPT, <<RequestId:32/little-integer>>}, {UserId, Socket}) ->
    log_serv:log("Request accept made by UID " ++ integer_to_list(UserId) ++ 
		     " for RID " ++ integer_to_list(RequestId)),
    usercom:request_accept(RequestId),
    {next_state, main, {UserId, Socket}};
main({?REQUEST_CANCEL, <<RequestId:32/little-integer>>}, {UserId, Socket}) ->
    log_serv:log("Request cancellation made by UID " ++ integer_to_list(UserId) ++
		     " for RID " ++ integer_to_list(RequestId)),
    usercom:request_cancel(RequestId),
    {next_state, main, {UserId, Socket}};
main(?GET_HOME_STATS, {UserId, Socket}) ->
    log_serv:log("Get home stats for UID " ++ integer_to_list(UserId)),
    UserStatsTableTuple = usercom:get_home_stats(UserId),
    UserStatsTablePack = packconv:pack(?GET_HOME_STATS, UserStatsTableTuple),
    ok = gen_tcp:send(Socket, UserStatsTablePack),
    {next_state, main, {UserId, Socket}};
main({?MATCH_START, <<RequestId:32/integer>>}, {UserId, Socket}) ->
    {match_table, MatchTable} = usercom:match(RequestId),
    ok = gen_tcp:send(Socket, ?MATCH_CONFIRM),
    {next_state, match, {UserId, Socket, MatchTable}}.


match({?MATCH_GPS, Packet}, {UserId, Socket, {MatchId, _,_,_,_} = MatchTable}) ->
    {Longitude, Latitude} = packconv:convert_pack(?MATCH_GPS, Packet),
    log_serv:log("Received GPS data from UID " ++ 
		     integer_to_list(UserId) ++ ": Long " ++ 
		     integer_to_list(Longitude) ++ " Lat " ++
		     integer_to_list(Latitude)),
    %usercom:gps_save(UserId, MatchId, Longitude, Latitude),
    {next_state, match, {UserId, Socket, MatchTable}};
match(?MATCH_STOP, {UserId, Socket, {MatchId, _,_,_,_}}) ->
    log_serv:log("Match stopped for MID " ++ MatchId),
    {next_state, main, {UserId, Socket}}.


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

handle_info({tcp, _, <<Type:2/binary>>}, State, {UserId, Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:State(Type, {UserId, Socket});
handle_info({tcp, _, <<Type:2/binary, Packet/binary>>}, State, {UserId, Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:State({Type, Packet}, {UserId, Socket});
handle_info({tcp_closed, _}, _State, {UserId, Socket}) ->
    log_serv:log("UID " ++ integer_to_list(UserId) ++ 
		     " disconnected unexpectedly"),    
    {stop, normal, {UserId, Socket}};
handle_info({tcp_error, _, _Reason}, _State, Socket) ->
    {ok, {Address, Port}} = inet:peername(Socket),
    log_serv:log("Tcp error for IP " ++ inet_parse:ntoa(Address) ++ 
		     " and Port " ++ integer_to_list(Port)),  
    {stop, normal, Socket}.


%% @doc The function is called upon termination and makes sure that
%% the Socket is closed.

-spec terminate(Reason, State, {UserId, Socket}) -> none() when
      Reason :: term(),
      State :: atom(),
      UserId :: integer(),
      Socket :: socket().

terminate(Reason, State, {UserId, Socket, _MatchTable}) ->
    ?MODULE:terminate(Reason, State, {UserId, Socket});
terminate(Reason, State, {UserId, _Socket}) ->
    ok = account:logout(UserId),
    log_serv:log("Logged out UID " ++ integer_to_list(UserId)),
    log_serv:log("Terminating client server with reason " ++
		     atom_to_list(Reason) ++ " in state " ++ 
		     atom_to_list(State)).
