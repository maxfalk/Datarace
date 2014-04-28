
%%====================================================================
%% Packet types
%% ------------------------------------------------------------------
%% This file contains macros to identify all login types. Use these
%% instead of constants when pattern matching packets et c.
%%====================================================================


%%====================================================================
%% Login
%%====================================================================

-define(LOGIN, <<0>>). %% Contains two 50 byte strings
-define(LOGIN_TRUE, <<0,0>>). 
-define(LOGIN_FALSE_USERNAME, <<0,1>>). 
-define(LOGIN_FALSE_PASSWORD, <<0,2>>). 
-define(LOGIN_LOGOUT, <<0,3>>).

%%====================================================================
%% Register
%%====================================================================

-define(REGISTER, <<1>>).
-define(REGISTER_TRUE, <<1,0>>).
-define(REGISTER_FALSE, <<1,1>>).
