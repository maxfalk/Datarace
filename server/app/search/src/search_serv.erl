%%@doc This module holds the server core for 
%%the user search server.
%%Author: Max Falk

-module(search_serv).

-export([start_link/0, stop/1, search/3]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

-include_lib("../include/types.hrl").
-include_lib("../include/database.hrl").

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%
%% SERVER API
%%%%%%%%%%%%%%%%%%%%

%%@doc Start a new process for the search_serv gen_server.
-spec start_link()-> Result when
      Result :: {ok,Pid} | ignore | {error,Error},
      Pid :: pid(),
      Error :: {already_started,Pid} | term().

start_link()->
    gen_server:start_link(?MODULE, [], []).
    
%%@doc Stop the gen_server.
-spec stop(Pid) -> ok when
      Pid :: pid().

stop(Pid)->
    gen_server:cast(Pid, stop).

%%@doc Search for a user like the one with the sent username
%% ordering by the best match first, filter att the user with id MyId.
-spec search(Pid, Username, MyId)-> [user_search_table(), ...] when
      Pid :: pid(),
      MyId :: integer(),
      Username :: string().

search(Pid, Username, MyId)->
    gen_server:call(Pid, {Username, MyId}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SERVER PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%@doc Search the database for the 5 best matches to the given 
%%username. 
-spec database_lookup(UserName, MyId) -> [user_search_table(), ...] when
      UserName :: string(),
      MyId :: integer().

database_lookup(UserName, MyId)->
    Sql_result = database:db_query(username_search,
				   <<"SELECT t1.id, t1.userName
                                      FROM
                                       tUsers t1
                                      WHERE
                                       t1.userName like ? and t1.id != ?
                                      LIMIT 5;">>,
				   [UserName ++ "%", MyId]),
    database:result_to_record(Sql_result, user_search_table).

   

%%@doc Searches the internally stored users for a match to the username given,
%% spawns a new preccess to do the search sends back a tuple,
%% {internal, lists of records}.
-spec search_internal_state(State, UserName)-> pid() when
      State :: [user_search_table(), ...],
      UserName :: string().

search_internal_state(State, UserName)->
    lists:reverse( 
      [Match || Match <- State, 
		string:equal(
		  string:left(
		    binary_to_list(
		      Match#user_search_table.userName), 
		    length(UserName)), 
		  UserName)]).

   

%%@doc Remove all elements in List1 which is not also members of List2.
-spec remove_duplicates(List1, List2)-> list() when
      List1 :: list(),
      List2 :: list().

remove_duplicates(List1, List2)->
    Set1 = sets:from_list(List1),
    Set2 = sets:from_list(List2),
    sets:to_list(sets:subtract(Set1,Set2)).



%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Init the server
-spec init(Options) -> {ok, term()} when
      Options :: list().

init(_Options)->
    {ok, []}.


%%@doc Terminate the server
-spec terminate(Reason, State)-> tbi when
      Reason :: term(),
      State :: term().


terminate(_Reason, _State)->
    ok.



%%@doc Handle search calls, search first the internal state for matches to the saerch then 
%% search the database for more matches. Remove duplicates and send them.
-spec handle_call(UserName, From, State)-> {noreply, NewState} when
      UserName :: string(),
      From :: {pid(), term()},
      State :: [user_search_table(), ...],
      NewState :: [user_search_table(), ...].

handle_call({UserName, MyId}, _From, State)->
    InternalMatches = search_internal_state(State, UserName),
    NewElements = remove_duplicates(database_lookup(UserName, MyId),InternalMatches),
    NewState = NewElements ++ lists:sublist(InternalMatches, ?MAXLEN - length(NewElements)),
    {reply, NewState, NewState}. 



%%@doc Handle cast to stop the server.
-spec handle_cast(stop, State) -> {stop, Reason, NewState} when
      State :: list(),
      Reason :: term(),
      NewState :: list().

handle_cast(stop, State)->
    {stop, normal, State}.
