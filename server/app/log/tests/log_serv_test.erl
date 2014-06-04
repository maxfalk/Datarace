-module(log_serv_test).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACTUAL TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_test()->
    log_serv:start_link(verbose),
    ?assert(log_serv:log("hej") =:= ok),
    ?assert(log_serv:log("2") =:= ok).  

