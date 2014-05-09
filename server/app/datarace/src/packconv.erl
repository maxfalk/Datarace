%%@doc Author: Max Falk Nilsson
%%This module holds functions for converting a incoming packet
%% to a more erlang esk form.

-module(packconv).

-export([convert_pack/2, pack/2]).

-include("../include/types.hrl").


%%@doc Convert a received message to an easier read format.
-spec convert_pack(Type, Data) -> Result when
      Type :: binary(),
      Data :: binary(),
      Result :: {[integer()], [integer()]} | 
		{[integer()], [integer()], [integer()]}.

convert_pack(Type, Data)->
    List_data = binary_to_list(Data),
    case Type of
	?LOGIN ->
	    login_pack(List_data);
	?REGISTER ->
	    register_pack(List_data)
    end.
    

%%@doc Convert a login packet.
-spec login_pack(Packet) -> {Username, Password} when
      Packet :: [integer()],
      Username :: [integer()],
      Password :: [integer()].

login_pack(List)->
    {Username, Password} = lists:split(50, List),
    {[X || X <- Username, X =/= 0], [X || X <- Password, X =/= 0]}.


%%@doc Convert a register packet.
-spec register_pack(Packet) -> {Username, Password, Email} when 
      Packet :: [integer()],
      Username :: [integer()],
      Password :: [integer()],
      Email :: [integer()].

register_pack(List)->
    {Username, Rest} = lists:split(50, List),
    {Password, Email} = lists:split(50, Rest),    
    {[X || X <- Username, X =/= 0], 
     [X || X <- Password, X =/= 0], 
     [X || X <- Email, X =/= 0]}.


pack(Type, Data) ->
    case Type of
	?REQUEST_LOOKUP ->
	    request_lookup_pack(Data);
	?GET_HOME_STATS_REPLY ->
	    get_home_stats_pack(Data)
    end.
    

request_lookup_pack(RequestTable) ->
    [ request_entry_pack(Entry) || Entry <- RequestTable ].

request_entry_pack({request_table, RequestId, ChallengeId, UserName,
		    {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}, 
		     State}) ->
    NamePad = 8*(50-byte_size(UserName)),
    <<?REQUEST_LOOKUP_REPLY/binary, %% 2 bytes
      RequestId:32/integer, %% 4 bytes
      ChallengeId:32/integer, %% 4 bytes
      UserName/binary, 0:NamePad, %% 50 bytes
      Year:32/integer, Month:32/integer, Day:32/integer, %% 12 bytes
      Hour:32/integer, Minute:32/integer, Second:32/integer, %% 12 bytes
      State:32/integer %% 4 bytes
    >>. %% 88 bytes in total
    

get_home_stats_pack({UserName, AverageSpeed, AverageDistance, 
		     Wins, Matches, Requests}) ->
    NamePad = 8*(50-byte_size(UserName)),
    <<?GET_HOME_STATS_REPLY/binary, %% 2 bytes 
      UserName/binary, 0:NamePad, %% 50 bytes
      AverageSpeed/float, %% 8 bytes
      AverageDistance/float, %% 8 bytes
      Wins/integer, %% 4 bytes
      Matches/integer, %% 4 bytes
      Requests/integer %% 4 bytes
    >>. %% 80 bytes in total

