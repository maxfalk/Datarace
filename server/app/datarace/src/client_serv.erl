
%%====================================================================
%% client_serv
%%====================================================================


-module(client_serv).
-behaviour(gen_fsm).

-export([start_link/0, verify_control_transfer/3]).
-export([init/1, verify/2, main/2, match/2, handle_info/3, terminate/3]).

-include("../include/types.hrl").
-include("../include/database.hrl").

-type socket() :: none().
-type datetime() :: {datetime, 
		     {{integer(), integer(), integer()}, 
		      {integer(), integer(), integer()}}}.
-type loop_data() :: {integer(), socket(), datetime(), match_table()}.

-record(loop_data, {user_id, socket, start_time, match_table = #match_table{}}).


%%====================================================================
%% Server API
%%====================================================================


%% @doc Starts a new Client Serv process. 
-spec start_link() -> Result when
      Error :: term(),
      Result :: {ok, pid()} | ignore | {error, Error}.

start_link() -> 
    gen_fsm:start_link(?MODULE, [], []). 


%% @doc Sends an asynchronous message to the Client Serv with pid Pid
%% telling it that it has the contol over a socket and may use it. 
%% Only relevant just after initialization, when Client_serv is in its
%% verify state. 
-spec verify_control_transfer(Pid, Socket, UserId) -> ok when
      Pid :: pid(),
      Socket :: socket(),
      UserId :: integer().

verify_control_transfer(Pid, Socket, UserId) -> 
    gen_fsm:send_event(Pid, {control_transferred, Socket, UserId}). 


%%====================================================================
%% Callback functions
%%====================================================================


%% @doc Initialize the Client Serv.
-spec init(Args) -> {ok, verify, LoopData} when
      UserId :: integer(),
      Socket :: socket(),
      Args :: {UserId, Socket},
      LoopData :: loop_data().

init(_Args) -> 
    process_flag(trap_exit, true),
    LoopData = #loop_data{},
    {ok, verify, LoopData}.


%% @doc Sends a LOGIN_TRUE message to the client when receiving a 
%% control_transferred event, informing the client about a successful 
%% login attempt.
-spec verify({control_transferred, Socket, UserId}, LoopData) -> Result when
      Socket :: socket(),
      UserId :: integer(),
      LoopData :: loop_data(),
      Result :: {next_state, main, LoopData}.

verify({control_transferred, Socket, UserId}, LoopData) -> 
    NewLoopData = LoopData#loop_data{socket = Socket, user_id = UserId}, 
    log_serv:log("Socket transfer for UID " ++ 
		     integer_to_list(NewLoopData#loop_data.user_id)),
    ok = gen_tcp:send(NewLoopData#loop_data.socket, ?LOGIN_TRUE),
    {next_state, main, NewLoopData}.


%% @doc Handles messages from a client during a session.
-spec main(Message, LoopData) -> Result when
      Type :: binary(),
      Package :: binary(),
      Message :: {Type, Package} | Type,
      LoopData :: loop_data(),      
      Result :: {stop, normal, LoopData}.

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

main(?REQUEST_NUMBER, LoopData) ->
    log_serv:log("Number of request lookup made by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id)),
    PendingRequests = usercom:get_num_pending_requests(LoopData#loop_data.user_id),
    ok = gen_tcp:send(LoopData#loop_data.socket, 
		      <<?REQUEST_NUMBER_REPLY/binary, 
			PendingRequests:32/little-integer>>),
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
    log_serv:log("Search was made by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++
		     " for '" ++ [X || X <- binary_to_list(Packet), X =/= 0] ++ "'"),
    case catch search_sup:start_search_serv() of
	{ok, Pid} ->
	    SearchString = packconv:convert_pack(?SEARCH_STRING, Packet),
	    SearchResult = search_serv:search(Pid, SearchString, LoopData#loop_data.user_id),
	    ResultPacket = packconv:pack(?SEARCH_RESULTS, SearchResult),
	    ok = gen_tcp:send(LoopData#loop_data.socket, ResultPacket),
	    search_serv:stop(Pid);
	_Error ->
	    ok = gen_tcp:send(LoopData#loop_data.socket, ?SEARCH_SERVER_DOWN)
    end,
    {next_state, main, LoopData};

main(?GET_HISTORY, LoopData) ->
    log_serv:log("Request for history by UID: " ++ 
		     integer_to_list(LoopData#loop_data.user_id)),
    History = usercom:get_history(LoopData#loop_data.user_id),
    Packet = packconv:pack(?GET_HISTORY, History),
    ok = gen_tcp:send(LoopData#loop_data.socket, Packet),
    {next_state, main, LoopData}.


%% @doc This state handles a match. It receives messages from the client 
%% such as GPS data, requests for the competitors current position and
%% requests to stop the match.
-spec match(Message, LoopData) -> Result when
      Type :: binary(),
      Package :: binary(),
      Message :: {Type, Package} | Type,
      LoopData :: loop_data(),      
      Result :: {stop, normal, LoopData} | 
		{next_state, main, LoopData} |
		{next_state, match, LoopData}.

match({?MATCH_GPS, Packet}, LoopData) ->
    {Longitude, Latitude} = packconv:convert_pack(?MATCH_GPS, Packet),
    log_serv:log("Received GPS data from UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++ ": " ++ 
		     float_to_list(Longitude) ++ ", " ++
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
    log_serv:log("Distance " ++ float_to_list(Distance) ++ 
		     " data sent to UID " ++
		     integer_to_list(LoopData#loop_data.user_id) ++
		     " for MID " ++ 
		     integer_to_list((LoopData#loop_data.match_table)#match_table.id)),
    {next_state, match, LoopData};

match(?MATCH_STOP, LoopData) ->
    log_serv:log("Match stopped by UID " ++ 
		     integer_to_list(LoopData#loop_data.user_id) ++ " for MID " ++ 
		     integer_to_list((LoopData#loop_data.match_table)#match_table.id)),
    usercom:match_stop(LoopData#loop_data.user_id, 
		       (LoopData#loop_data.match_table)#match_table.id,
		       (LoopData#loop_data.match_table)#match_table.requestId),
    MatchStats = usercom:get_match_end_stats((LoopData#loop_data.match_table)#match_table.id),
    Packet = packconv:pack(?MATCH_STOP, MatchStats),
    ok = gen_tcp:send(LoopData#loop_data.socket, Packet),
    {next_state, main, LoopData}.


%% @doc Receives TCP packets and forwards them to the function 
%% representing the current state or stops if the message is 
%% erroneous.
-spec handle_info(Event, State, LoopData) -> Result when
      Event :: {tcp, Socket, Packet} | 
	       {tcp_closed, Socket} | 
	       {tcp_error, Socket, Reason},
      State :: term(),
      LoopData :: loop_data(),
      Socket :: socket(),
      Packet :: binary(),
      Reason :: term(),
      Result :: none() | {stop, normal, LoopData}.

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
-spec terminate(Reason, State, LoopData) -> none() when
      Reason :: term(),
      State :: atom(),
      LoopData :: loop_data().

terminate(_Reason, State, LoopData) ->
    account:logout(LoopData#loop_data.user_id),
    log_serv:log("Logged out UID " ++ integer_to_list(LoopData#loop_data.user_id)),
    log_serv:log("Terminating client server in state " ++ atom_to_list(State)).
