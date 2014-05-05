
-module(date_serv).

-export([start/1]).

start(Pid)->
    {Date, _} = calendar:local_time(),
    loop(Pid, Date).

send_msg(Pid)->
    gen_server:cast(Pid, open_new_file).
    
loop(Pid, CurrentDate)->
    {Date, _} = calendar:local_time(),
    case CurrentDate of 
	Date ->
	    loop(Pid, CurrentDate);
	_ ->
	    send_msg(Pid),
	    loop(Pid, Date)
    end.
    
    
