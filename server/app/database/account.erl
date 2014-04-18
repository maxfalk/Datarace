%%@doc Author: Max Falk Nilsson
%% This module contains functions for interacting with 
%% the databases' accounting operations. Sush as logging in to
%% the system, loggin out and registering a new user.
%%
-module(account).

-export([login/2,logout/1,register/3]).

%%@doc Records to hold database collected data.
-record(login_table, {id :: integer(), salt :: string(), password :: string()}).
-record(loginlog_table, {id :: integer()}).
-record(register_table,{id :: integer()}).

-type login_table() :: {integer(), string(), string()}.
-type loginlog_table() :: {integer()}.
-type register_table() :: {integer()}.

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
    Login_rec = get_row(User_data,1),
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
    emysql:prepare(login_user_info,
		   <<"SELECT id, salt, password from tUsers WHERE user_name = ?">>),
    Sql_result =  emysql:execute(database_pool, login_user_info, [User_name]),
    emysql:as_record(Sql_result, login_table, record_info(fields,login_table)).
    
%%@doc Gets the nth row of data from a list.
-spec get_row(List, Num) -> any() | {error,no_item} when
      List :: list(),
      Num :: integer().


get_row(List, Num) when length(List) > 0->
    lists:nth(Num,List);
get_row(_List, _Num) ->
    {error, no_item}.

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
    emysql:prepare(login_log_login,
		   <<"INSERT INTO tLoginLog (userId,login) VALUES(?, now())">>),
    emysql:execute(database_pool, login_log_login, [Userid]),
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
    emysql:prepare(loginlog_logout_select,
		   <<"SELECT max(id) as id FROM tLoginLog WHERE userId = ?">>),
    Sql_result =  emysql:execute(database_pool, loginlog_logout_select, [Userid]),
    get_row(emysql:as_record(Sql_result, 
				     loginlog_table, record_info(fields,loginlog_table)),1).
    

%%@doc Set the time when the user logged out in the database.
-spec set_logout_time(LoginlogId) -> ok when
      LoginlogId :: integer().

set_logout_time(LoginlogId)->
    emysql:prepare(loginlog_logout_update,
		   <<"UPDATE tLoginLog SET logout = now() WHERE id = ?">>),
    emysql:execute(database_pool, loginlog_logout_update, [LoginlogId]),
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
    emysql:prepare(register_insert,
		   <<"INSERT INTO tUsers (user_name, email, register_date,password,salt) 
                      VALUES(?,?,now(),?,?)">>),
    emysql:execute(database_pool, register_insert, [User_name,Email,Password,Salt]),
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
    emysql:prepare(register_select,
		   <<"SELECT id FROM tUsers WHERE user_name = ?">>),
    Sql_result = emysql:execute(database_pool, register_select, [User_name]),
    get_row(emysql:as_record(Sql_result,register_table,record_info(fields,register_table)),1).
    
    


    
