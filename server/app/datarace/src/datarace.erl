%%Appliction for the server core, datarace server.

-module(datarace).

-behaviour(application).

-export([start/2,stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Start the datarace server
-spec start(Type, List)-> ok when
      Type :: atom(),
      List :: any().

start(normal, [])->
    database:init(),
    master_sup:start_link().

%%@doc Stop the datarace server
-spec stop(Type)-> ok when
      Type :: atom().

stop(_)->
    Ref = erlang:monitor(process, whereis(master_sup)),
    exit(whereis(master_sup), shutdown),
    receive
	{'DOWN', Ref, process, _Pid, Reason} ->
	    database:stop()
    end,
    ok.
