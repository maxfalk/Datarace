
%%====================================================================
%% Packet types
%% ------------------------------------------------------------------
%% This file contains macros to identify all login types. Use these
%% instead of constants when pattern matching packets et c.
%%====================================================================


%%====================================================================
%% Login
%%====================================================================

-define(LOGIN, <<0>>). %% Contains two 50 byte strings: uname, pword
-define(LOGIN_TRUE, <<0,0>>). 
-define(LOGIN_FALSE_USERNAME, <<0,1>>). 
-define(LOGIN_FALSE_PASSWORD, <<0,2>>). 
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

-define(REQUEST, <<2,0>>). %% Contains two u32ints: challengeId, distance 
-define(REQUEST_LOOKUP, <<2,1>>). 
-define(REQUEST_ACCEPT, <<2,2>>). %% Contains one u32int: requestId
-define(REQUEST_CANCEL, <<2,3>>). %% Contains one u32int: requestId
-define(REQUEST_LOOKUP_REPLY, <<2,4>>). %% Contains: lots 
-define(REQUEST_HOME_STATS, <<2,5>>).


%%====================================================================
%% Get data
%%====================================================================

-define(GET_HOME_STATS, <<3,0>>).
-define(GET_HOME_STATS_REPLY, <<3,1>>).
