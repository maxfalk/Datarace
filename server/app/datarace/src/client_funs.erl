
%%====================================================================
%% client_funs
%%====================================================================


-module(client_funs).

-export([connect/2, 
	 login/3, 
	 register/4,
	 request/3,
	 request_lookup/1,
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


%% @doc Gets status for a new and made race requests.
-spec request_lookup(Socket) -> Result when
      Socket :: socket(),
      Result :: ok | {error, Reason},
      Reason :: closed | term().

request_lookup(Socket) ->
    gen_tcp:send(Socket, ?REQUEST_LOOKUP).


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

