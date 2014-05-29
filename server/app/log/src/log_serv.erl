%%Author:Max Falk
%%@doc Log messages to file and if started as verbose to 
%%the screen aswell.

-module(log_serv).

-export([start_link/1, log/1, stop/0]).
-export([init/1, handle_cast/2, terminate/2]).

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SERVER API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Start log server with the specified options,verbose or normal.
-spec start_link(Options :: normal | verbose)-> {ok, pid()} | ignore | {error, atom()}.

start_link(Options)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%@doc stop the server.
-spec stop()-> ok.

stop()->
    gen_server:cast(?MODULE, stop).


%%@doc Log a message. Writes the message to file and if started in verbose the the 
%%screen.
-spec log(Msg :: string()) -> ok.

log(Msg) when is_list(Msg)->
    gen_server:cast(?MODULE, Msg);
log(Msg) ->
    gen_server:cast(?MODULE, "Can't log the sent message").
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% SERVER PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Formats the message for logging, adds timestamp in the beggining and ends 
%%it with a new line.
-spec make_log_msg(Msg :: string()) -> string().

make_log_msg(Msg) ->
    {{Year, Month, Day}, {Hour, Minute, Seconds}} = calendar:local_time(),
    Date = integer_to_list(Year) ++ "-" ++ 
			       integer_to_list(Month) ++ "-" ++ integer_to_list(Day),
    Time = integer_to_list(Hour) ++ ":" ++ 
			       integer_to_list(Minute) ++ ":" ++ integer_to_list(Seconds),
    "[" ++ Date ++ " " ++ Time  ++ "] " ++ Msg ++ "\n".

%%@doc Make a new filename for todays date
-spec make_filename()-> string().

make_filename()->
    {{Year, Month, Day}, _} = calendar:local_time(),
    ("../log/datarace_log_" ++ integer_to_list(Year) ++ "_" ++ integer_to_list(Month) ++ 
	 "_" ++ integer_to_list(Day) ++ ".log").

%%@doc Write to String to the file Filename with the mode Mode.
-spec write(Filename :: string(), String :: string(), Mode :: [atom(), ...]) ->
	       ok | {error, atom()}.

write(Filename, String, Mode)->
    file:write_file(Filename, String, Mode).

%%@doc Open the file with path Filename with mode Mode
-spec open(Filename :: string(), Mode :: [atom(), ...])-> {ok, term()} | {error, atom()}.

open(Filename, Mode)->
    file:open(Filename, Mode).

%%@doc Close the file with name Filename.
-spec close(Filename :: string())-> ok | {error, atom()}.

close(Filename)->
    file:close(Filename).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Initiate the server, start necassary help process and open the first file to
%%write to, make the file if necassary.
-spec init(Options :: atom())-> {ok, {string(), tuple(), atom()}}.

init(Options)->
    {Current_date, _} = calendar:local_time(),
    Filename = make_filename(),
    open(Filename, [append]),
    spawn_link(fun ()-> date_serv:start(self()) end),
    {ok, {Filename, Current_date, Options}}.

%%@doc Terminate the log server.
-spec terminate(_Reason :: atom(), {Filename :: string(), _Options :: atom()})->
    ok | {error, atom()}.

terminate(_Reason, {Filename, _Options})->
    close(Filename).


%%@doc Handle casted message to the server. Print recieved messages. Make a new
%%file if the requested too.
-spec handle_cast(term(),{string(), tuple(), atom()})-> 
			 {noreply, tuple()} | 
			 {stop, normal, tuple()}.
	

handle_cast(open_new_file, {Filename, _Old_date, Options})->
    New_filename = make_filename(),
    {Current_date, _} = calendar:local_time(),
    close(Filename),
    open(New_filename, [append]),
    {noreply,{New_filename, Current_date, Options}};    
handle_cast(stop, Loopdata)->
    {stop, normal, Loopdata};
handle_cast(Msg, {Filename, _Current_date, verbose} = Loopdata)->
    Log_string = make_log_msg(Msg), 
    write(Filename, Log_string, [append]),
    io:format(Log_string),
    {noreply, Loopdata};
handle_cast(Msg, {Filename, _Date, normal} = Loopdata)->
    Log_string = make_log_msg(Msg), 
    write(Filename, Log_string, [append]),
    {noreply, Loopdata}.


