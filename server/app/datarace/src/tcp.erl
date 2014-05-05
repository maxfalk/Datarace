

-module(tcp).

-export([listen/1, accept/1, recv/1, connect/2, send/2, send_register/4, 
	 send_login/3, quick_accept/1]).


listen(Port) ->
    gen_tcp:listen(Port, [binary, inet, {active, once}, {packet, 4}]).

accept(ListenSocket) ->
    gen_tcp:accept(ListenSocket).

recv(Socket) ->
    receive
	Request ->
	    Request
    after 5000 ->
	    inet:setopts(Socket, [{active, false}, {packet, raw}]),
	    Request = gen_tcp:recv(Socket, 0),
	    inet:setopts(Socket, [{packet, 4}])
    end,
    inet:setopts(Socket, [{active, once}]),
    Request.

connect(Address, Port) ->
    gen_tcp:connect(Address, Port, [{packet, 4}]).

send(Socket, Data) ->
    gen_tcp:send(Socket, Data).


quick_accept(Port) ->
    {ok, ListenSocket} = listen(Port),
    {ok, Socket} = accept(ListenSocket),
    Socket.

send_login(Socket, Username, Password) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    Packet = list_to_binary([0] ++ Un ++ Pw),
    send(Socket, Packet).

send_register(Socket, Username, Password, Email) ->
    Un = Username ++ [0 || _ <- lists:seq(1, 50 - length(Username))],
    Pw = Password ++ [0 || _ <- lists:seq(1, 50 - length(Password))],
    Em = Email ++ [0 || _ <- lists:seq(1, 50 - length(Email))],
    Packet = list_to_binary([1] ++ Un ++ Pw ++ Em),
    send(Socket, Packet).
