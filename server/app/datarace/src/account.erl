%%@doc Author: Max Falk Nilsson
%% This module contains functions for interacting with 
%% the databases' accounting operations. Sush as logging in to
%% the system, loggin out and registering a new user.
%% !! NOT FINISHED !!
-module(account).

-export([login/2,logout/1,register/3]).

-include("../include/database.hrl").


%%@doc Looks at the user and password combinations if it is valid 
%% and matches a register user.
-spec login(User_name, Password) -> ok | {error, no_user} | {error, wrong_password} when
      User_name :: string(),
      Password :: string().

  
login(User_name, Password)->
    User_data = get_login_data(User_name),  
    login_helper(Password, User_data).

%%@doc Looks at the users data from the database and matches it with the
%%input data to se if it is vaild.
-spec login_helper(Password, User_data) ->  ok | {error, no_user} | {error, wrong_password} when
      Password :: string(),
      User_data :: login_table(). 

login_helper(_Password, [])->
    %%logg attempeted loggin?
    {error, no_user};
login_helper(Password, User_data)->
    Login_rec = database:get_row(User_data,1),
    Password_salt = Password ++ binary_to_list(Login_rec#login_table.salt),
    io:format("User logging in: ~p , ~p~n",[Login_rec#login_table.id,Password_salt]),
    case check_password(Password_salt ,Login_rec#login_table.password) of
	ok ->
	    set_loggedin(Login_rec#login_table.id),
	    {ok, Login_rec#login_table.id};
	false ->
	    %%Set login attempt?
	    {error, wrong_password}
    end.
    
    
%%@doc Get necessary data about the user from the database
-spec get_login_data(User_name) -> [login_table(), ...] when
      User_name :: string().

get_login_data(User_name)->
    Sql_result = database:db_query(login_user_info, 
			    <<"SELECT id, salt, password from tUsers WHERE user_name = ?">>,
		 [User_name]),
			       database:result_to_record(Sql_result, login_table).

    

%%@doc Checks if two passwords match the input password with salt un hashed and
%%the stored password hashed.
-spec check_password(Password_input :: string(),Password_stored :: string()) -> ok | false.

check_password(Password_input,Password_stored) ->
    Password = crypt(Password_input),
    if
	 Password == Password_stored ->
	    ok;
	true ->
	    false
    end.

%%@doc Hash a given password.
-spec crypt(Password) -> string() when
      Password :: string().

crypt(Password)->
    base64:encode(crypto:sha(lists:sublist(Password,1,50))).


%%@doc Mark user as loggedin in the database
-spec set_loggedin(Userid) -> ok when
      Userid :: integer().

set_loggedin(Userid)->
    database:db_query(login_log_login,
		   <<"INSERT INTO tLoginLog (userId,login) VALUES(?, now())">>,
		   [Userid]),
    ok.


%%@doc Logout user from database, makes appropriate calls to define user
%% as logged out.
-spec logout(Userid) -> ok when
      Userid :: integer().

logout(Userid)->
    set_loggedout(Userid),
    ok.

%@doc Mark user as logged out in the database.
-spec set_loggedout(Userid) -> ok when
      Userid :: integer().

set_loggedout(Userid)->
    Loginlog_rec = get_user_lastlogin(Userid),
    set_logout_time(Loginlog_rec#loginlog_table.id).

%%@doc Get the last time the user logged in.
-spec get_user_lastlogin(Userid) ->loginlog_table() when
      Userid :: integer().
    
get_user_lastlogin(Userid)->
    Sql_result = database:db_query(loginlog_logout_select,
				<<"SELECT max(id) as id FROM tLoginLog WHERE userId = ?">>,
				[Userid]),
    database:get_row(database:result_to_record(Sql_result, 
					       loginlog_table),1).
    

%%@doc Set the time when the user logged out in the database.
-spec set_logout_time(LoginlogId) -> ok when
      LoginlogId :: integer().

set_logout_time(LoginlogId)->
    database:db_query(loginlog_logout_update,
		   <<"UPDATE tLoginLog SET logout = now() WHERE id = ?">>,
		   [LoginlogId]),
    ok.
    

%%@doc Register a new user in the database, checks that the user doesn't already exist.
-spec register(User_name, Password, Email) -> ok | {error, user_already_exist} when
      User_name :: string(),
      Password :: string(),
      Email :: string().

register(User_name, Password, Email)->
    case check_user_exists(User_name) of
	false ->
	    Salt = base64:encode(crypto:strong_rand_bytes(50)),
	    register_user(User_name,crypt(Password ++ binary_to_list(Salt)),Email, Salt),
	    ok;
	true ->
	    {error, user_already_exist}
    end.


%%@doc Register user in the database.
-spec register_user(User_name,Password,Email,Salt) -> ok when
      User_name :: string(),
      Password :: string(),
      Email :: string(),
      Salt :: string().

register_user(User_name,Password,Email,Salt)->
    database:db_query(register_insert,
		   <<"INSERT INTO tUsers (user_name, email, register_date,password,salt) 
                      VALUES(?,?,now(),?,?)">>,
		  [User_name,Email,Password,Salt]),
    ok.
  
%%@doc Check if user already exist, is already register in the DB.
-spec check_user_exists(User_name)-> true | false when
      User_name :: string().

check_user_exists(User_name)->
    Check_fun = fun(List) when length(List) == 0->
			true;
		   (_List) ->
			false
		end,
    Check_fun(get_user(User_name)).
    
	    
	    
%%@doc Get users information, for checking that no other user is already regisered
%%with the given name.
-spec get_user(User_name) -> register_table() when
      User_name :: string().

get_user(User_name)->
    Sql_result = database:db_query(register_select,
				<<"SELECT id FROM tUsers WHERE user_name = ?">>,
				[User_name]),
    database:get_row(database:result_to_record(Sql_result, register_table), 1).
    
    


    
