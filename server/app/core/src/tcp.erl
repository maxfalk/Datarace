

-module(tcp).

-export([listen/1, accept/1, recv/1, connect/2, send/2, quick_accept/1]).


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
