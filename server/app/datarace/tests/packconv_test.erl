%%Tests for the account module. 
%%
%%
%%
-module(packconv_test).

-include ("../include/types.hrl").
-include_lib ("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%
%% ACTUALL TESTS    %%
%%%%%%%%%%%%%%%%%%%%%%

convert_pack_test() ->
    List = [ 0 || _ <- lists:seq(1,100) ], 
    ?assertEqual(packconv:convert_pack(?LOGIN, List), {"", ""} ),
    ?assertEqual(packconv:convert_pack(?REGISTER, List), {"", "", ""} ).
    


