

-module(tcp).

-export([listen/1, 
	 accept/1, 
	 recv/1, 
	 connect/2, 
	 send/2, 
	 binary_login/2, 
	 binary_register/3,
	 close/1]).


listen(Port) ->
    gen_tcp:listen(Port, [binary, inet, {active, once}, {packet, 4}]).

accept(ListenSocket) ->
    gen_tcp:accept(ListenSocket).

recv(Socket) ->
    receive
	{tcp, _Socket, Packet} ->
	    inet:setopts(Socket, [{active, once}]),
	    {tcp, Packet};
	Message ->
	    Message
    after 1000 ->
	    {error, no_reply}
    end.

connect(Address, Port) ->
    gen_tcp:connect(Address, Port, [{packet, 4}]).

send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

binary_login(Username, Password) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    list_to_binary([0] ++ Un ++ Pw).

binary_register(Username, Password, Email) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    Em = Email ++ [0 || _ <- lists:seq(1, 50 - length(Email))],
    list_to_binary([1] ++ Un ++ Pw ++ Em).

close(Socket) ->
    gen_tcp:close(Socket).
