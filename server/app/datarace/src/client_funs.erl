
%%====================================================================
%% client_funs
%%====================================================================

-module(client_funs).

-export([connect/2, 
	 login/3, 
	 register/4, 
	 close/1]).

-include("../include/types.hrl").


%%====================================================================
%% Exported functions
%%====================================================================

connect(Address, Port) ->
    gen_tcp:connect(Address, Port, [binary, inet, {packet, 4}, {active, true}]).


login(Socket, Username, Password) ->
    LoginPacket = login_pack(Username, Password),
    gen_tcp:send(Socket, LoginPacket).


register(Socket, Username, Password, Email) ->
    RegisterPacket = register_pack(Username, Password, Email),
    gen_tcp:send(Socket, RegisterPacket).


close(Socket) ->
    gen_tcp:close(Socket).


%%====================================================================
%% Functions for packing
%%====================================================================

login_pack(Username, Password) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    list_to_binary([0] ++ Un ++ Pw).


register_pack(Username, Password, Email) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    Em = Email ++ [0 || _ <- lists:seq(1, 50 - length(Email))],
    list_to_binary([1] ++ Un ++ Pw ++ Em).


%%====================================================================
%% Functions for unpacking
%%====================================================================


