
%%====================================================================
%% client_funs
%%====================================================================


-module(client_funs).

-export([connect/2, 
	 login/3, 
	 register/4,
	 request/3,
	 request_lookup/1,
	 request_number/1,
	 search_string/2,
	 get_history/1,
	 get_home_stats/1,
	 request_accept/2,
	 start_match/2,
	 gps_match/3,
	 pos_match/1,
	 stop_match/1,
	 logout/1,
	 close/1]).

-include("../include/types.hrl").

-type socket() :: none().


%%====================================================================
%% Exported functions
%%====================================================================

%% @doc Create a TCP connection with the settings binary, inet, 
%% packet = 4 and active = true.
-spec connect(Address, Port) -> Result when
      Address :: {0..255, 0..255, 0..255, 0..255} | localhost,
      Port :: integer(),
      Result :: {ok, pid()} | {error, Reason},
      Reason :: term().

connect(Address, Port) ->
    gen_tcp:connect(Address, Port, [binary, inet, {packet, 4}, {active, true}]).


%% @doc Log in to the datarace server.
-spec login(Socket, Username, Password) -> Result when
      Socket :: socket(),
      Username :: list(),
      Password :: list(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

login(Socket, Username, Password) ->
    LoginPacket = login_pack(Username, Password),
    gen_tcp:send(Socket, LoginPacket).


%% @doc Register on the datarace server.
-spec register(Socket, Username, Password, Email) -> Result when 
      Socket :: socket(),
      Username :: list(),
      Password :: list(),
      Email :: list(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

register(Socket, Username, Password, Email) ->
    RegisterPacket = register_pack(Username, Password, Email),
    gen_tcp:send(Socket, RegisterPacket).


%% @doc Make a race request to another user on the datarace server.
-spec request(Socket, ChallengeId, Distance) -> Result when
      Socket :: socket(),
      ChallengeId :: integer(),
      Distance :: integer(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().
    
request(Socket, ChallengeId, Distance) ->
    RequestPacket = request_pack(ChallengeId, Distance),
    gen_tcp:send(Socket, RequestPacket).


%% @doc Gets status for a new and made race requests for a logged in user.
-spec request_lookup(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

request_lookup(Socket) ->
    gen_tcp:send(Socket, ?REQUEST_LOOKUP).


%% @doc Gets the number of requests for a logged in user.
-spec request_number(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

request_number(Socket) ->
    gen_tcp:send(Socket, ?REQUEST_NUMBER).


%% @doc Gets stats for a logged in user.
-spec get_home_stats(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

get_home_stats(Socket) ->
    gen_tcp:send(Socket, ?GET_HOME_STATS).


%% @doc Gets status for a new and made race requests.
-spec search_string(Socket, SearchString) -> Result when
      Socket :: socket(),
      SearchString :: list(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

search_string(Socket, SearchString) ->
    SearchBinary = list_to_binary(SearchString),
    StringLength = (8*length(SearchString)),
    SearchPacket = <<?SEARCH_STRING/binary,
		     SearchBinary/binary, 
		     0:StringLength>>,
    gen_tcp:send(Socket, SearchPacket).


%% @doc Accept a match request from another user.
-spec request_accept(Socket, RequestId) -> Result when
      Socket :: socket(),
      RequestId :: integer(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

request_accept(Socket, RequestId) ->
    gen_tcp:send(Socket, <<?REQUEST_ACCEPT/binary,
			   RequestId:32/little-integer>>).


%% @doc Start a match.
-spec start_match(Socket, RequestId) -> Result when
      Socket :: socket(),
      RequestId :: integer(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

start_match(Socket, RequestId) ->
    gen_tcp:send(Socket, <<?MATCH_START/binary, 
			   RequestId:32/little-integer>>).


%% @doc Stop a running match. Note that a match MUST be started.
-spec stop_match(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

stop_match(Socket) ->
    gen_tcp:send(Socket, ?MATCH_STOP).



%% @doc Send GPS data to the server during a match. 
%% Note that a match MUST be started.
-spec gps_match(Socket, Latitude, Longitude) -> Result when
      Socket :: socket(),
      Latitude :: float(),
      Longitude :: float(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

gps_match(Socket, Latitude, Longitude) ->
    gen_tcp:send(Socket, <<?MATCH_GPS/binary, 
			   Latitude/little-float,
			   Longitude/little-float>>).

%% @doc Get current (soft real time) position from opponent during a 
%% match. Note that a match MUST be started.
-spec pos_match(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

pos_match(Socket) ->
    gen_tcp:send(Socket, ?MATCH_COMP_POS).


%% @doc Get match hisory. Contains statistics and stuff.
-spec get_history(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

get_history(Socket) ->
    gen_tcp:send(Socket, ?GET_HISTORY).


%% @doc Log out and disconnect.
-spec logout(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

logout(Socket) ->
    gen_tcp:send(Socket, ?LOGIN_LOGOUT).


%% @doc Close a connection.
-spec close(Socket) -> ok when
      Socket :: socket().

close(Socket) ->
    gen_tcp:close(Socket).


%%====================================================================
%% Functions for packing
%%====================================================================


%% @doc Create a binary packet out of login information.
-spec login_pack(Username, Password) -> Packet when
      Username :: list(),
      Password :: list(),
      Packet :: binary().

login_pack(Username, Password) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    list_to_binary([0] ++ Un ++ Pw).


%% @doc Create a binary packet out of register information.
-spec register_pack(Username, Password, Email) -> Packet when
      Username :: list(),
      Password :: list(),
      Email :: list(), 
      Packet :: binary().

register_pack(Username, Password, Email) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    Em = Email ++ [0 || _ <- lists:seq(1, 50 - length(Email))],
    list_to_binary([1] ++ Un ++ Pw ++ Em).


%% @doc Create a request packet out of request information.
-spec request_pack(ChallengeId, Distance) -> Packet when
      ChallengeId :: integer(),
      Distance :: integer(),
      Packet :: binary().

request_pack(ChallengeId, Distance) ->
   <<?REQUEST/binary, 
     ChallengeId:32/little-integer, 
     Distance:32/little-integer>>.


%%====================================================================
%% Functions for unpacking
%%====================================================================
