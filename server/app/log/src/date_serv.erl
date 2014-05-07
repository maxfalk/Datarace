%%@doc Author: Max Falk Nilsson
%% This module announces the date of the day
%% and saves it in the process pid. It lets us know
%% what day it is and when a new day has arrived.


-module(date_serv).

-export([start/1]).

%%@doc============================================================%
%% Gives us the date of the day and a pid for that specific date.
%=================================================================%

-spec start(Pid)-> ok when
	Pid :: pid().

start(Pid)->
    {Date, _} = calendar:local_time(),
    loop(Pid, Date).

%%@doc==========================================================%
%% Sends the atom open_new_file to the process pid.
%===============================================================%

-spec send_msg(Pid)-> ok when
	Pid :: pid().

send_msg(Pid)->
    gen_server:cast(Pid, open_new_file).
    

%%@doc=========================================================%
%% Compares if the todays date is the current date, otherwise sends
%% message that a new date has arrived. 
%==============================================================%

-spec loop(Pid, CurrentDate)-> ok when
	Pid :: pid(),
	CurrentDate :: {integer(), integer(), integer()}.

loop(Pid, CurrentDate)->
    {Date, _} = calendar:local_time(),
    case CurrentDate of 
	Date ->
	    loop(Pid, CurrentDate);
	_ ->
	    send_msg(Pid),
	    loop(Pid, Date)
    end.
    
    
    
