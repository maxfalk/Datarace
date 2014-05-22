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
-spec login(UserName, Password) -> ok | {error, no_user} | {error, wrong_password} when
      UserName :: string(),
      Password :: string().

  
login(UserName, Password)->
    UserData = get_user_data(UserName),  
    login_helper(Password, UserData).

%%@doc Looks at the users data from the database and matches it with the
%%input data to se if it is vaild.
-spec login_helper(Password, Login_rec) ->  {ok, integer()} | {error, no_user} | {error, wrong_password} when
      Password :: string(),
      Login_rec :: login_table(). 

login_helper(_Password, {error,no_item})->
    %%logg attempeted loggin?
    {error, no_user};
login_helper(Password, Login_rec)->
    PasswordSalt = Password ++ binary_to_list(Login_rec#login_table.salt),
    case check_password(PasswordSalt, Login_rec#login_table.password) of
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
-spec get_user_data(UserName) -> [login_table(), ...] when
      UserName :: string().

get_user_data(UserName)->
    Sql_result = database:db_query(login_user_info, 
			     <<"SELECT id, salt, password from tUsers WHERE userName = ?">>,
				   [UserName]),
    database:get_row(database:result_to_record(Sql_result, login_table), 1).

    

%%@doc Checks if two passwords match the input password with salt un hashed and
%%the stored password hashed.
-spec check_password(PasswordInput :: string(),PasswordStored :: string()) -> ok | false.

check_password(PasswordInput,PasswordStored) ->
    Password = crypt(PasswordInput),
    if
	 Password == PasswordStored ->
	    ok;
	true ->
	    false
    end.


%%@doc Mark user as loggedin in the database
-spec set_loggedin(UserId) -> ok when
      UserId :: integer().

set_loggedin(UserId)->
    database:db_query(login_log_login,
		   <<"INSERT INTO tLoginLog (userId,login) VALUES(?, now())">>,
		   [UserId]),
    ok.

%%@doc Check if user is already loggedin.
-spec check_user_already_login(UserId) -> boolean() when
      UserId :: integer().

check_user_already_login(UserId)-> 
    Loginlogrec = get_user_lastlogin(UserId),
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
-spec logout(UserId | UserName) -> ok when
      UserId :: integer(),
      UserName :: string().

logout(UserId) when is_integer(UserId) ->
    set_loggedout(UserId),
    ok;
logout(UserName) when is_list(UserName) ->
    UserData = database:get_row(get_user_data(UserName),1),
    case UserData of
	{error, no_item}->
	    ok;
	_ ->
	    set_loggedout(UserData#login_table.id),
	    ok
    end.

%@doc Mark user as logged out in the database.
-spec set_loggedout(UserId) -> ok when
      UserId :: integer().

set_loggedout(UserId)->
    Loginlog_rec = get_user_lastlogin(UserId),
    set_logout_time(Loginlog_rec#loginlog_table.id).

%%@doc Get the last time the user logged in.
-spec get_user_lastlogin(UserId) ->loginlog_table() | {error, no_item} when
      UserId :: integer().
    
get_user_lastlogin(UserId)->
    Sql_result = database:db_query(loginlog_logout_select,
				<<"SELECT max(id) as id FROM tLoginLog WHERE userId = ?">>,
				[UserId]),
    database:get_row(database:result_to_record(Sql_result, 
					       loginlog_table),1).
    

%%@doc Set the time when the user logged out in the database.
-spec set_logout_time(LoginlogId) -> ok when
      LoginlogId :: integer().

set_logout_time(LoginlogId)->
    database:async_db_query(loginlog_logout_update,
		   <<"UPDATE tLoginLog SET logout = now() WHERE id = ?">>,
		   [LoginlogId]),
    ok.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           REGISTER               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Register a new user in the database, checks that the user doesn't already exist.
-spec register(UserName, Password, Email) -> ok | {error, user_already_exist} when
      UserName :: string(),
      Password :: string(),
      Email :: string().

register(UserName, Password, Email)->
    case check_user_exists(UserName) of
	user_not_found ->
	    Salt = base64:encode(crypto:strong_rand_bytes(50)),
	    register_user(UserName,crypt(Password ++ binary_to_list(Salt)),Email, Salt);
	user_found ->
	    {error, user_already_exist}
    end.


%%@doc Register user in the database.
-spec register_user(UserName,Password,Email,Salt) -> ok when
      UserName :: string(),
      Password :: string(),
      Email :: string(),
      Salt :: string().

register_user(UserName,Password,Email,Salt)->
    database:async_db_query(register_insert,
		   <<"INSERT INTO tUsers (userName, email, registerDate,password,salt) 
                      VALUES(?,?,now(),?,?)">>,
		  [UserName,Email,Password,Salt]),
    ok.
  
%%@doc Check if user already exist, is already register in the DB.
-spec check_user_exists(UserName)-> user_found | user_not_found when
      UserName :: string().

check_user_exists(UserName)->
    Check_fun = fun(List) when length(List) == 0 ->
			user_not_found;
		   (_List) ->
			user_found
		end,
    Check_fun(get_user(UserName)).
    
 
	    
%%@doc Get users information, for checking that no other user is already regisered
%%with the given name.
-spec get_user(UserName) -> [register_table(), ...] when
      UserName :: string().

get_user(UserName)->
    Sql_result = database:db_query(register_select,
				<<"SELECT id FROM tUsers WHERE userName = ?">>,
				[UserName]),
    database:result_to_record(Sql_result, register_table).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           DELETE                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Delete a user
-spec delete(Userid)-> ok when 
      Userid :: integer().


delete(UserName) when is_list(UserName) ->
    Data = get_user(UserName),
    [delete(X#register_table.id) || X<- Data],
    ok;
delete(Userid)->    
    database:async_db_query(delete_user,
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


    
