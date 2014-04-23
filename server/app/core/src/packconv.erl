%%@doc Author: Max Falk Nilsson
%%This module holds functions for converting a incoming packet
%% to a more erlang esk form.

-module(packconv).

-export([convert_pack/2]).

%%@doc convert a received message to a easier read format
%%
%%

convert_pack(Type,Data)->
    List_data = binary_to_list(Data),
    case Type of
	0 ->
	    login_pack(List_data)
    end.
    

%%@doc Convert a login packet
%%
%%

login_pack(List)->
    {Username, Password} = lists:split(50,List),
    {[X || X <- Username, X =/= 0], [X || X <- Password, X =/= 0]}.


