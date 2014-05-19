%%@doc Author: Max Falk Nilsson
%% This module contains functions for interacting with 
%% the databases' accounting operations. Sush as logging in to
%% the system, loggin out and registering a new user.
%% !! NOT FINISHED !!
-module(account).

-export([get_user_lastlogin/1,login/2,logout/1,register/3,delete/1]).

-include("../include/database.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           LOGIN                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%@doc Looks at the user and password combinations if it is valid 
%% and matches a register user.
-spec login(User_name, Password) -> ok | {error, no_user} | {error, wrong_password} when
      User_name :: string(),
      Password :: string().

  
login(User_name, Password)->
    User_data = get_user_data(User_name),  
    login_helper(Password, User_data).

%%@doc Looks at the users data from the database and matches it with the
%%input data to se if it is vaild.
-spec login_helper(Password, User_data) ->  {ok, integer()} | {error, no_user} | {error, wrong_password} when
      Password :: string(),
      User_data :: [login_table(), ...]. 

login_helper(_Password, [])->
    %%logg attempeted loggin?
    {error, no_user};
login_helper(Password, User_data)->
    Login_rec = database:get_row(User_data,1),
    Password_salt = Password ++ binary_to_list(Login_rec#login_table.salt),
    case check_password(Password_salt, Login_rec#login_table.password) of
	ok ->
	    (case check_user_already_login(Login_rec#login_table.id) of
		false ->
		     {error, already_loggedin};
		true->
		     set_loggedin(Login_rec#login_table.id),
		     {ok, Login_rec#login_table.id}
	     end);
	false ->
	    %%Set login attempt?
	    {error, wrong_password}
    end.
    
    
%%@doc Get necessary data about the user from the database
-spec get_user_data(User_name) -> [login_table(), ...] when
      User_name :: string().

get_user_data(User_name)->
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


%%@doc Mark user as loggedin in the database
-spec set_loggedin(Userid) -> ok when
      Userid :: integer().

set_loggedin(Userid)->
    database:db_query(login_log_login,
		   <<"INSERT INTO tLoginLog (userId,login) VALUES(?, now())">>,
		   [Userid]),
    ok.

%%@doc Check if user is already loggedin.
-spec check_user_already_login(Userid) -> boolean() when
      Userid :: integer().

check_user_already_login(Userid)-> 
    Loginlogrec = get_user_lastlogin(Userid),
    case Loginlogrec#loginlog_table.id of
	undefined ->
	    true;
	{error, no_item}->
	    true;
	_Num ->
	    {_, _, _, R, _} = database:db_query(login_log_check,
						<<"SELECT id, login, logout
                                                   FROM
                                                    tLoginLog t1
                                                   WHERE
                                                     t1.id = ? and
                                                     t1.logout is not null">>,
						[Loginlogrec#loginlog_table.id]),
	    R =/= []
    end.
	
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           LOGOUT                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%@doc Logout user from database, makes appropriate calls to define user
%% as logged out.
-spec logout(Userid | Username) -> ok when
      Userid :: integer(),
      Username :: string().

logout(Userid) when is_integer(Userid) ->
    set_loggedout(Userid),
    ok;
logout(Username) when is_list(Username) ->
    Userdata = database:get_row(get_user_data(Username),1),
    set_loggedout(Userdata#login_table.id),
    ok.

%@doc Mark user as logged out in the database.
-spec set_loggedout(Userid) -> ok when
      Userid :: integer().

set_loggedout(Userid)->
    Loginlog_rec = get_user_lastlogin(Userid),
    set_logout_time(Loginlog_rec#loginlog_table.id).

%%@doc Get the last time the user logged in.
-spec get_user_lastlogin(Userid) ->loginlog_table() | {error, no_item} when
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
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           REGISTER               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Register a new user in the database, checks that the user doesn't already exist.
-spec register(User_name, Password, Email) -> ok | {error, user_already_exist} when
      User_name :: string(),
      Password :: string(),
      Email :: string().

register(User_name, Password, Email)->
    case check_user_exists(User_name) of
	user_not_found ->
	    Salt = base64:encode(crypto:strong_rand_bytes(50)),
	    register_user(User_name,crypt(Password ++ binary_to_list(Salt)),Email, Salt);
	user_found ->
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
-spec check_user_exists(User_name)-> user_found | user_not_found when
      User_name :: string().

check_user_exists(User_name)->
    Check_fun = fun(List) when length(List) == 0 ->
			user_not_found;
		   (_List) ->
			user_found
		end,
    Check_fun(get_user(User_name)).
    
	    
	    
%%@doc Get users information, for checking that no other user is already regisered
%%with the given name.
-spec get_user(User_name) -> [register_table(), ...] when
      User_name :: string().

get_user(User_name)->
    Sql_result = database:db_query(register_select,
				<<"SELECT id FROM tUsers WHERE user_name = ?">>,
				[User_name]),
    database:result_to_record(Sql_result, register_table).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           DELETE                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Delete a user
-spec delete(Userid)-> ok when 
      Userid :: integer().


delete(Username) when is_list(Username) ->
    Data = get_user(Username),
    delete(Data#register_table.id);
delete(Userid)->    
    database:db_query(delete_user,
		      <<"CALL delete_user(?)">>,
		      [Userid]),
    ok.



    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           UTILS                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%@doc Hash a given password.
-spec crypt(Password) -> string() when
      Password :: string().

crypt(Password)->
    base64:encode(crypto:sha(lists:sublist(Password,1,50))).


    
