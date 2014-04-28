%%@doc Author: Max Falk Nilsson
%%This module holds functions for converting a incoming packet
%% to a more erlang esk form.

-module(packconv).

-export([convert_pack/2]).

-include("../include/types.hrl").


%%@doc convert a received message to an easier read format.
%%
%%

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
%%
%%

-spec login_pack(Packet) -> {Username, Password} when
      Packet :: [integer()],
      Username :: [integer()],
      Password :: [integer()].

login_pack(List)->
    {Username, Password} = lists:split(50, List),
    {[X || X <- Username, X =/= 0], [X || X <- Password, X =/= 0]}.


%%@doc Convert a register packet.
%%
%%

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

