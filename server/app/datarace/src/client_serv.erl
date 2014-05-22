
%%====================================================================
%% Client_serv
%%====================================================================

-module(client_serv).
-behaviour(gen_fsm).

-export([start_link/2, verify_control_transfer/1]).
-export([init/1, verify/2, main/2, match/2, handle_info/3, terminate/3]).

-include("../include/types.hrl").
-include("../include/database.hrl").

-type socket() :: none().

-record(loop_data, {user_id, socket, start_time, match_table = #match_table{}}).


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

init({UserId, Socket}) ->
    process_flag(trap_exit, true),
    LoopData = #loop_data{socket = Socket, user_id = UserId},
    log_serv:log("Spawned new client_serv for UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id)),
    {ok, verify, LoopData}.


%% @doc Sends a LOGIN_TRUE message to the client when receiving a 
%% control_transferred event, informing the client about a successful 
%% login attempt.

-spec verify(control_transferred, {UserId, Socket}) -> Result when
      UserId :: integer(),
      Socket :: socket(),
      Result :: {next_state, main, {UserId, Socket}}.

verify(control_transferred, LoopData) ->
    log_serv:log("Socket transfer for UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id)),
    ok = gen_tcp:send(LoopData#loop_data.socket, ?LOGIN_TRUE),
    {next_state, main, LoopData}.


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
main({?REQUEST, <<ChallengeId:32/little-integer, Distance:32/little-integer>>}, LoopData) ->
    log_serv:log("Match request made by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++ " for " ++
		     integer_to_list(ChallengeId) ++ " distance " ++
		     integer_to_list(Distance)),
    usercom:request(LoopData#loop_data.user_id, ChallengeId, Distance),
    {next_state, main, LoopData};
main(?REQUEST_LOOKUP, LoopData) ->
    log_serv:log("Match request lookup made by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id)),
    RequestTableTuple = usercom:request_lookup(LoopData#loop_data.user_id),
    {MadePack, ChalPack} = packconv:pack(?REQUEST_LOOKUP, RequestTableTuple),
    ok = gen_tcp:send(LoopData#loop_data.socket, MadePack),
    ok = gen_tcp:send(LoopData#loop_data.socket, ChalPack),
    {next_state, main, LoopData};
main({?REQUEST_ACCEPT, <<RequestId:32/little-integer>>}, LoopData) ->
    log_serv:log("Request accept made by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++ 
		     " for RID " ++ integer_to_list(RequestId)),
    usercom:request_accept(RequestId),
    {next_state, main, LoopData};
main({?REQUEST_CANCEL, <<RequestId:32/little-integer>>}, LoopData) ->
    log_serv:log("Request cancellation made by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++
		     " for RID " ++ integer_to_list(RequestId)),
    usercom:request_cancel(RequestId),
    {next_state, main, LoopData};
main(?GET_HOME_STATS, LoopData) ->
    log_serv:log("Get home stats for UID " ++ integer_to_list(LoopData#loop_data.user_id)),
    UserStatsTableTuple = usercom:get_home_stats(LoopData#loop_data.user_id),
    UserStatsTablePack = packconv:pack(?GET_HOME_STATS, UserStatsTableTuple),
    ok = gen_tcp:send(LoopData#loop_data.socket, UserStatsTablePack),
    {next_state, main, LoopData};
main({?MATCH_START, <<RequestId:32/little-integer>>}, LoopData) ->
    log_serv:log("Match started with RID " ++ integer_to_list(RequestId)),
    MatchTable = usercom:match(RequestId),
    ok = gen_tcp:send(LoopData#loop_data.socket, ?MATCH_CONFIRM),
    NewLoopData = LoopData#loop_data{start_time = calendar:local_time(), 
				     match_table = MatchTable},
    {next_state, match, NewLoopData};
main({?SEARCH_STRING, Packet}, LoopData) ->
    case search_sup:start_search_serv() of
	{ok, Pid} ->
	    SearchString = packconv:convert_pack(?SEARCH_STRING, Packet),
	    SearchResult = search_serv:search(Pid, SearchString),
	    ResultPacket = packconv:pack(?SEARCH_RESULTS, SearchResult),
	    ok = gen_tcp:send(LoopData#loop_data.socket, ResultPacket),
	    search_serv:stop(Pid);
	_Error ->
	    ok = gen_tcp:send(LoopData#loop_data.socket, ?SEARCH_SERVER_DOWN)
    end,
    {next_state, main, LoopData}.


match({?MATCH_GPS, Packet}, LoopData) ->
    {Longitude, Latitude} = packconv:convert_pack(?MATCH_GPS, Packet),
    log_serv:log("Received GPS data from UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++ ": Long " ++ 
		     float_to_list(Longitude) ++ ", Lat " ++
		     float_to_list(Latitude)),
    usercom:gps_save(LoopData#loop_data.user_id, 
		     LoopData#loop_data.match_table#match_table.id, 
		     Longitude, Latitude),
    {next_state, match, LoopData};
match(?MATCH_COMP_POS, LoopData) ->
    ChallengerId = (LoopData#loop_data.match_table)#match_table.userId, 
    Distance = gps:calc_pointdistance(ChallengerId, 
				      (LoopData#loop_data.match_table)#match_table.id, 
				      LoopData#loop_data.start_time),
    ok = gen_tcp:send(LoopData#loop_data.socket, 
		      <<?MATCH_COMP_REPLY/binary, Distance/little-float>>),
    log_serv:log("Distance data sent to " ++
		     integer_to_list(LoopData#loop_data.user_id) ++
		     "for MID " ++ 
		     integer_to_list((LoopData#loop_data.match_table)#match_table.id)),
    {next_state, match, LoopData};
match(?MATCH_STOP, LoopData) ->
    log_serv:log("Match stopped for MID " ++ 
		     integer_to_list((LoopData#loop_data.match_table)#match_table.id)),
    {next_state, main, LoopData}.






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

handle_info({tcp, _, <<Type:2/binary>>}, State, LoopData) ->
    inet:setopts(LoopData#loop_data.socket, [{active, once}]),
    ?MODULE:State(Type, LoopData);
handle_info({tcp, _, <<Type:2/binary, Packet/binary>>}, State, LoopData) ->
    inet:setopts(LoopData#loop_data.socket, [{active, once}]),
    ?MODULE:State({Type, Packet}, LoopData);
handle_info({tcp_closed, _}, _State, LoopData) ->
    log_serv:log("UID " ++ integer_to_list(LoopData#loop_data.user_id) ++ 
		     " disconnected unexpectedly"),    
    {stop, normal, LoopData};
handle_info({tcp_error, _, _Reason}, _State, LoopData) ->
    {ok, {Address, Port}} = inet:peername(LoopData#loop_data.socket),
    log_serv:log("Tcp error for IP " ++ inet_parse:ntoa(Address) ++ 
		     " and Port " ++ integer_to_list(Port)),  
    {stop, normal, LoopData}.


%% @doc The function is called upon termination and makes sure that
%% the Socket is closed.

-spec terminate(Reason, State, {UserId, Socket}) -> none() when
      Reason :: term(),
      State :: atom(),
      UserId :: integer(),
      Socket :: socket().

terminate(Reason, State, LoopData) ->
    ok = account:logout(LoopData#loop_data.user_id),
    log_serv:log("Logged out UID " ++ integer_to_list(LoopData#loop_data.user_id)),
    log_serv:log("Terminating client server in state " ++ atom_to_list(State)).
