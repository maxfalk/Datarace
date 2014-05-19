%%@doc This module holds the server core for 
%%the user search server.
%%
%% 
%%
%%Author: Max Falk

-module(search_serv).

-export([start_link/0, stop/1, search/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

-include_lib("../include/types.hrl").
-include_lib("../include/database.hrl").

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%
%% SERVER API
%%%%%%%%%%%%%%%%%%%%

start_link()->
    gen_server:start_link(?MODULE, [], []).
    
stop(Pid)->
    gen_server:cast(Pid, stop).


search(Pid, Username)->
    gen_server:call(Pid, Username).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SERVER PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Search the database for the 10 best matches to the given 
%%username. 
-spec database_lookup(UserName) -> [user_search_table(), ...] when
      UserName :: string().

database_lookup(UserName)->
    Sql_result = database:db_query(username_search,
				   <<"SELECT t1.id, t1.user_name
                                     FROM
                                      tUsers t1
                                     WHERE
                                      t1.user_name like ?
                                     LIMIT 10;">>,
				   [UserName ++ "%"]),
    database:result_to_record(Sql_result, user_search_table).

%%@doc Searches the internally stored users for a match to the username given.
-spec search_internal_state(State, UserName)-> [user_search_table(), ...] when
      State :: [user_search_table(), ...],
      UserName :: string().

search_internal_state(State, UserName)->
    [Match || Match <- State, 
	      string:equal(
		string:left(
		  binary_to_list(Match#user_search_table.user_name), length(UserName)), 
		UserName)].

%%@doc Remove all elements in List1 which is not also members of List2.
-spec remove_duplicates(List1, List2)-> list() when
      List1 :: list(),
      List2 :: list().

remove_duplicates(List1, List2)->
    Set1 = sets:from_list(List1),
    Set2 = sets:from_list(List2),
    sets:to_list(sets:subtract(Set1,Set2)).


%%@doc Send Packet to Pid if there is something to send.
-spec send(Pid, Packet) -> Packet when
      Packet :: any(),
      Pid :: pid().

send(From, Packet) when length(Packet) > 0->
    gen_server:reply(From, Packet);
send(_From, Packet) ->
    Packet.

%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%

%%@doc Init the server
%%
-spec init(Options) -> {ok, term()} when
      Options :: list().

init(_Options)->
    {ok, []}.


%%@doc Terminate the server
%%
-spec terminate(Reason, State)-> tbi when
      Reason :: term(),
      State :: term().


terminate(_Reason, _State)->
    ok.



%%@doc Handle search calls
%%
-spec handle_call(UserName, From, State)-> {noreply, NewState} when
      UserName :: string(),
      From :: {pid(), term()},
      State :: [user_search_table(), ...],
      NewState :: [user_search_table(), ...].

handle_call(UserName, From, State)->
    InternalMatches = search_internal_state(State, UserName),
    send(From, InternalMatches),
    NewElements = remove_duplicates(database_lookup(UserName),InternalMatches),
    send(From, NewElements),
    {noreply, NewElements ++ lists:sublist(InternalMatches, ?MAXLEN - length(NewElements))}.



%%@doc handle cast
%%
-spec handle_cast(stop, State) -> {stop, Reason, NewState} when
      State :: list(),
      Reason :: term(),
      NewState :: list().

handle_cast(stop, State)->
    {stop, normal, State}.
