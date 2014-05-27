

%%====================================================================
%% Packet types
%% ------------------------------------------------------------------
%% This file contains macros to identify all packet types. Use these
%% instead of constants when pattern matching packets et c.
%%====================================================================


%%====================================================================
%% Login
%%====================================================================

-define(LOGIN, <<0>>). %% Contains two 50 byte strings: uname, pword
-define(LOGIN_TRUE, <<0,0>>). 
-define(LOGIN_FALSE_USERNAME, <<0,1>>). 
-define(LOGIN_FALSE_PASSWORD, <<0,2>>).
-define(LOGIN_FALSE_LOGGED_IN, <<0,4>>).
-define(LOGIN_LOGOUT, <<0,3>>).


%%====================================================================
%% Register
%%====================================================================

-define(REGISTER, <<1>>). %% Contains three 50 byte strings; uname, pword, email
-define(REGISTER_TRUE, <<1,0>>).
-define(REGISTER_FALSE, <<1,1>>).


%%====================================================================
%% Requests
%%====================================================================

-define(REQUEST, <<2,0>>). %% Contains two int32: challengeId, distance 
-define(REQUEST_LOOKUP, <<2,1>>). %% Empty
-define(REQUEST_ACCEPT, <<2,2>>). %% Contains one int32: requestId
-define(REQUEST_CANCEL, <<2,3>>). %% Contains one int32: requestId
-define(REQUEST_LOOKUP_REPLY_MADE, <<2,4>>). %% Contains: lots 
-define(REQUEST_LOOKUP_REPLY_CHAL, <<2,5>>). %% Contains: lots 


%%====================================================================
%% Get data
%%====================================================================

-define(GET_HOME_STATS, <<3,0>>).
-define(GET_HOME_STATS_REPLY, <<3,1>>).


%%====================================================================
%% Match
%%====================================================================

-define(MATCH_START, <<4,0>>).
-define(MATCH_CONFIRM, <<4,1>>).
-define(MATCH_GPS, <<4,2>>). %% Contains two floats: longitude, latitude 
-define(MATCH_STOP, <<4,3>>).
-define(MATCH_COMP_POS, <<4,4>>).
-define(MATCH_COMP_REPLY, <<4,5>>). %% Contains one float: distance


%%====================================================================
%% Search
%%====================================================================

-define(SEARCH_STRING, <<5,0>>).
-define(SEARCH_RESULTS, <<5,1>>).
-define(SEARCH_SERVER_DOWN, <<5,2>>).
