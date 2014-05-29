%% @doc This module holds functions for converting incoming and 
%% outgoing network packets

-module(packconv).

-export([convert_pack/2, pack/2]).

-include("../include/types.hrl").
-include("../include/database.hrl").


%%====================================================================
%% Exported functions
%%====================================================================


%% @doc Convert a received binary message to a tuple.
-spec convert_pack(Type, Data) -> Result when
      Type :: binary(),
      Data :: binary(),
      Result :: term().

convert_pack(Type, Data)->
    ListData = binary_to_list(Data),
    case Type of
	?LOGIN ->
	    login_pack(ListData);
	?REGISTER ->
	    register_pack(ListData);
	?MATCH_GPS ->
	    match_gps_pack(Data);
	?SEARCH_STRING ->
	    search_pack(ListData)
    end.


%% @doc Convert a set of data to a binary packet
-spec pack(Type, Data) -> Result when
      Type :: binary(),
      Data :: term(),
      Result :: binary() | {binary(), binary()}.

pack(Type, Data) ->
    case Type of
	?REQUEST_LOOKUP ->
	    request_lookup_pack(Data);
	?GET_HOME_STATS ->
	    get_home_stats_pack(Data);
	?SEARCH_RESULTS ->
	    search_results_pack(Data);
	?GET_HISTORY ->
	    get_history_pack(Data);
	?MATCH_STOP ->
	    get_match_end_pack(Data)
    end.


%%====================================================================
%% Local functions for convert_pack
%%====================================================================

%% @doc Convert a login packet.
-spec login_pack(Packet) -> {Username, Password} when
      Packet :: binary(),
      Username :: list(),
      Password :: list().

login_pack(List)->
    {Username, Password} = lists:split(50, List),
    {[X || X <- Username, X =/= 0], [X || X <- Password, X =/= 0]}.


%% @doc Convert a register packet.
-spec register_pack(Packet) -> {Username, Password, Email} when 
      Packet :: binary(),
      Username :: list(),
      Password :: list(),
      Email :: list().

register_pack(List) ->
    {Username, Rest} = lists:split(50, List),
    {Password, Email} = lists:split(50, Rest),    
    {[X || X <- Username, X =/= 0], 
     [X || X <- Password, X =/= 0], 
     [X || X <- Email, X =/= 0]}.


%% @doc Convert a GPS packet.
-spec match_gps_pack(Data) -> {Longitude, Latitude} when
      Data :: binary(),
      Longitude :: float(),
      Latitude :: float().

match_gps_pack(Data) ->
    <<Longitude/little-float, Latitude/little-float>> = Data,
    {Longitude, Latitude}.


%% @doc Convert a search packet.
-spec search_pack(SearchBinary) -> SearchString when
      SearchBinary :: binary(),
      SearchString :: list().

search_pack(SearchString) ->
    [X || X <- SearchString, X =/= 0].
    

%%====================================================================
%% Local functions for pack
%%====================================================================


%% @doc Make a binary packet out of request lookup results
-spec request_lookup_pack(RequestLookup) -> Result when
      MadeRequestTable :: [request_table()],
      ReceivedRequestTable :: [request_table()],
      MadeRequestBinary :: binary(),
      ReceivedRequestBinary :: binary(),      
      RequestLookup :: {MadeRequestTable, ReceivedRequestTable},
      Result :: {MadeRequestBinary, ReceivedRequestBinary}.


request_lookup_pack({MadeRequestTable, ReceivedRequestTable}) ->
    MadeRequestPack = [ request_entry_pack(Entry) || Entry <- MadeRequestTable ],
    ReceivedRequestPack = [ request_entry_pack(Entry) || Entry <- ReceivedRequestTable ],
    {binary_join(?REQUEST_LOOKUP_REPLY_MADE, MadeRequestPack), 
     binary_join(?REQUEST_LOOKUP_REPLY_CHAL, ReceivedRequestPack)}.


%% @doc Make a binary packet out of a request_table.
-spec request_entry_pack(RequestTable) -> Result when
      RequestTable :: request_table(),
      Result :: binary().

request_entry_pack({request_table, RequestId, ChallengeId, UserName,
		    {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}, 
		     State, Distance}) ->
    NamePad = 8*(50-byte_size(UserName)),
    <<RequestId:32/little-integer, %% 4 bytes
      ChallengeId:32/little-integer, %% 4 bytes
      UserName/binary, 0:NamePad, %% 50 bytes
      Year:32/little-integer, Month:32/little-integer, Day:32/little-integer, %% 12 bytes
      Hour:32/little-integer, Minute:32/little-integer, Second:32/little-integer, %% 12 bytes
      State:32/little-integer, %% 4 bytes
      Distance:32/little-integer %% 4 bytes
    >>. %% 90 bytes in total


%% @doc Make a binary packet out of a user stats table.
-spec get_home_stats_pack(UserStatsTable) -> Result when
      UserStatsTable :: user_stats_table(),
      Result :: binary().

get_home_stats_pack({user_stats_table, UserName, AverageSpeed, AverageDistance, 
		     Wins, Matches, Requests}) ->
    NamePad = 8*(50-byte_size(UserName)),
    <<?GET_HOME_STATS_REPLY/binary, %% 2 bytes 
      UserName/binary, 0:NamePad, %% 50 bytes
      AverageSpeed/little-float, %% 8 bytes
      AverageDistance/little-float, %% 8 bytes
      Wins:32/little-integer, %% 4 bytes
      Matches:32/little-integer, %% 4 bytes
      Requests:32/little-integer %% 4 bytes
    >>. %% 80 bytes in total


%% @doc Make a binary packet out of a search result
-spec search_results_pack(SearchResults) -> Result when
      SearchResults :: [user_search_table()],
      Result :: binary().

search_results_pack(Data) ->
    Packets = [ <<UserId:32/little-integer, Username/binary, 0:(8*(50-byte_size(Username)))>> 
		    || {user_search_table, UserId, Username} <- Data ],
    binary_join(?SEARCH_RESULTS, Packets).


%% @doc Make a binary packet out of history results.
-spec get_history_pack(History) -> Result when
      History :: [match_stats_table()],  			      
      Result :: binary().

get_history_pack(History) ->
    PackList = [ match_stats(X) || X <- History ],
    binary_join(?GET_HISTORY_REPLY, PackList).


%% @doc Make a binary packet out of match end results. 
-spec get_match_end_pack(MatchStats) -> Result when
      MatchStats :: [match_stats_table()],
      Result :: binary().

get_match_end_pack(MatchStats) ->
    PackList = [ match_stats(X) || X <- MatchStats ],
    binary_join(?MATCH_STOP_REPLY, PackList).


%% @doc Make a binary packet out of a match_stats_table,
-spec match_stats(MatchStatsTable) -> Result when
      MatchStatsTable :: match_stats_table(),
      Result :: binary().

match_stats({match_stats_table, UserId, Time, Winner, Distance, AverageSpeed, State}) -> 
    <<UserId:32/little-integer, % 4 bytes
      Time:32/little-integer, % 4 bytes
      Winner:32/little-integer, % 4 bytes
      Distance:32/little-integer, % 4 bytes
      AverageSpeed/little-float, % 8 bytes
      State:32/little-integer % 4 bytes
    >>. % 28 bytes total
    

%% @doc Join a list of binaries into a single binary.
-spec binary_join(Type, [Binary]) -> Result when
      Type :: binary(),
      Binary :: binary(),
      Result :: binary().

binary_join(Type, []) ->    
    Type;
binary_join(Type, List) ->
    lists:foldl(fun (Value, Acc) ->
			<<Acc/binary, Value/binary>> 
		end, 
		Type, List).
