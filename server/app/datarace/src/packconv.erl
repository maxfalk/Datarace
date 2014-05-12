%%@doc Author: Max Falk Nilsson
%%This module holds functions for converting a incoming packet
%% to a more Erlang-esque form.

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


%%@doc Convert a register packet to tuple.
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


%%@doc Convert a set of data to a binary packet

pack(Type, Data) ->
    case Type of
	?REQUEST_LOOKUP ->
	    request_lookup_pack(Data);
	?GET_HOME_STATS ->
	    get_home_stats_pack(Data)
    end.
    

request_lookup_pack({MadeRequestTable, ReceivedRequestTable}) ->
    {[ request_entry_pack(Entry) || Entry <- MadeRequestTable ],
     [ request_entry_pack(Entry) || Entry <- ReceivedRequestTable ]}.

request_entry_pack({request_table, RequestId, ChallengeId, UserName,
		    {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}, 
		     State}) ->
    NamePad = 8*(50-byte_size(UserName)),
    <<?REQUEST_LOOKUP_REPLY/binary, %% 2 bytes
      RequestId:32/little-integer, %% 4 bytes
      ChallengeId:32/little-integer, %% 4 bytes
      UserName/binary, 0:NamePad, %% 50 bytes
      Year:32/little-integer, Month:32/little-integer, Day:32/little-integer, %% 12 bytes
      Hour:32/little-integer, Minute:32/little-integer, Second:32/little-integer, %% 12 bytes
      State:32/little-integer %% 4 bytes
    >>. %% 88 bytes in total
    

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


%%@doc Join a list of binaries into a single binary.
-spec binary_join([Binary]) -> Result when
      Binary :: binary(),
      Result :: binary().

binary_join([]) ->    
    <<>>;
binary_join([Part]) ->
    Part;
binary_join([Head|Tail]) ->
    lists:foldr(fun (Value, Acc) ->
			<<Acc/binary, Value/binary>> 
		end, 
		Head, Tail).
