%% @doc Case server module for managing case records.
%%
%% This module implements a GenServer responsible for managing legal case records. 
%% It provides functionalities to start the server, edit and clear case items, 
%% retrieve case data, and generate unique case identifiers.
%%
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
-module(case_server).
-behavior(gen_server).


%% @doc Generates a unique case ID.
-export([generate_id/0]).

%% @doc Starts the case server.
-export([start_link/0, start_link/1]).

%% @doc Edits an item in the case.
-export([edit_case_item/2]).

%% @doc Clears a specific item from the case.
-export([clear_case_item/1]).

%% @doc Retrieves the entire case.
-export([get_case/0]).

%% @doc Retrieves a specific item from the case.
-export([get_case_item/1]).

%% @doc GenServer callbacks.
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% @doc Starts the case server with an initial case state.
%%
%% This function initializes the `case_server` process using the given case 
%% data as the initial state.
%%
%% @param Case The initial case state.
%% @return {ok, Pid} on success, where Pid is the process identifier 
%%         of the started server.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single process start operation.
start_link(Case)->
    gen_server:start_link({local, ok}, case_server, Case, []).

%% @doc Starts the case server with an empty initial state.
%%
%% This function initializes the `case_server` process with an empty 
%% initial state.
%%
%% @return {ok, Pid} on success, where Pid is the process identifier 
%%         of the started server.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single process start operation.
start_link()->
    gen_server:start_link({local, ok}, case_server, {}, []).

%% @doc Updates a specific case item.
%%
%% This function sends an asynchronous cast request to update a particular key 
%% in the case record with the provided value.
%%
%% @param CaseItem The new value to be assigned to the key.
%% @param Key The key in the case record that needs to be updated.
%% @return ok (asynchronous operation, no direct response).
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single message send operation.
edit_case_item(CaseItem, Key)->
    gen_server:cast(ok, {edit_case_item, {CaseItem, Key}}).

%% @doc Removes a specific key from the case.
%%
%% This function sends an asynchronous cast request to remove a particular key 
%% from the case record.
%%
%% @param Key The key to be removed from the case record.
%% @return ok (asynchronous operation, no direct response).
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single message send operation.
clear_case_item(Key) ->
    gen_server:cast(ok, {remove, Key}).

%% @doc Retrieves the entire case record.
%%
%% This function sends a synchronous call to the GenServer to fetch the full 
%% case record.
%%
%% @return The complete case record as a map.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it returns the current state.
get_case() ->
    gen_server:call(ok, get_case).

%% @doc Retrieves a specific item from the case.
%%
%% This function sends a synchronous call to the GenServer to retrieve a value 
%% associated with a given key in the case record.
%%
%% @param Key The key whose value needs to be retrieved.
%% @return The value of the key, or `"not found"` if the key does not exist.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single map lookup.
get_case_item(Key) ->
    gen_server:call(ok, {get_case_item, Key}).

%% @doc Initializes the case server with an empty or predefined state.
%%
%% This function initializes the `case_server` with either an empty case 
%% (default values) or a predefined case with given attributes.
%%
%% @param {} Initializes an empty case with default values.
%% @param {Title, Status, Lawyer_id, Client_ids, Documents} Initializes a case 
%%        with the specified attributes.
%% @return {ok, Case} where Case is the initial state of the case server.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it only creates a map with initial values.
init({}) ->
    {ok, #{id=> generate_id(), title=> undefined, status=> pending, lawyer_id=> undefined, client_id=> [], documents=> [], created_at => os:timestamp(), updated_at=> os:timestamp()}};
init({Title, Status, Lawyer_id, Client_ids, Documents}) ->
    {ok, #{id=> generate_id(), title=> Title, status=> Status, lawyer_id=> Lawyer_id, client_id=> Client_ids, documents=> Documents, created_at => os:timestamp(), updated_at=> os:timestamp()}}.

%% @doc Handles the update of a case item.
%% 
%% This function updates a specific key in the case record with the given 
%% case item and updates the timestamp to reflect the modification.
%%
%% @param CaseItem The new value to be assigned to the key.
%% @param Key The key in the case record that needs to be updated.
%% @param Case The existing case record.
%% @return {noreply, NewCase} where NewCase is the updated case record.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single update operation.
handle_cast({edit_case_item, {CaseItem, Key}}, Case) ->
    NewCase = Case#{Key => CaseItem, updated_at=>os:timestamp()},
    {noreply, NewCase};

%% @doc Handles the removal of a specific key from a case record.
%%
%% Depending on the key, this function assigns either `undefined` or an 
%% empty list to the key and updates the timestamp to reflect the change.
%%
%% @param Key The key to be removed from the case record.
%% @param Case The existing case record.
%% @return {noreply, NewCase} where NewCase is the updated case record.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single update operation.
handle_cast({remove, Key}, Case) ->
    case Key of
        status->
            {noreply, Case#{Key => undefined, updated_at=>os:timestamp()}};
        lawyer_id ->
            {noreply, Case#{Key => undefined, updated_at=>os:timestamp()}};
        title ->
            {noreply, Case#{Key => undefined, updated_at=>os:timestamp()}};
        client_id->
            {noreply, Case#{Key=> [], updated_at=> os:timestamp()}};
        documents -> 
            {noreply, Case#{Key=> [], updated_at=> os:timestamp()}};
        _ ->
            {noreply, Case} end.

%% @doc Retrieves a specific item from the case.
%%
%% This function looks up a given key in the case record. If the key 
%% exists, it returns the corresponding value; otherwise, it returns `"not found"`.
%%
%% @param Key The key whose value needs to be retrieved.
%% @param _From The process that made the request (ignored).
%% @param Case The current state of the case.
%% @return {reply, Value, Case} where Value is the retrieved case item or `"not found"`.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single map lookup.
handle_call({get_case_item, Key}, _From, Case)->
    {reply, maps:get(Key, Case, "not found"), Case};

%% @doc Retrieves the entire case record.
%%
%% This function returns the complete case record stored in the server state.
%%
%% @param _From The process that made the request (ignored).
%% @param Case The current state of the case.
%% @return {reply, Case, Case} where Case is the complete case record.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it returns the current state.
handle_call(get_case, _From, Case) ->
    {reply, Case, Case};

%% @doc Stops the case server gracefully.
%%
%% This function shuts down the `case_server` process normally.
%%
%% @param _From The process that made the request (ignored).
%% @param Case The current state of the case.
%% @return {stop, normal, ok, Case} indicating a normal shutdown.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it performs a single stop operation.
handle_call(stop, _From, Case) ->
    {stop, normal, ok, Case}.

%% @doc Handles the termination of the case server.
%%
%% This function is called when the `case_server` process is shutting down.
%% It prints a message indicating that the case process is stopping.
%%
%% @param _Reason The reason for termination (ignored).
%% @param _Case The final state of the case before termination (ignored).
%% @return ok to indicate successful termination handling.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), as it only performs an IO operation.
terminate(_Reason, _Case)->
    io:format("stopping case process~n"),
    ok.

%% @doc Generates a random 5-character alphanumeric ID.
%%
%% This function creates a unique identifier by selecting random characters 
%% from a predefined character set consisting of uppercase letters, lowercase 
%% letters, and digits.
%%
%% @return A list of 5 randomly selected characters forming a unique ID.
%% @author Jaron Andrus
%% @version 0.5
%% @since 2025-03-18
%% @complexity O(1), because the id will always be a predefined amount
%%                     of digits.
generate_id() -> 
    Charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    IdLength = 5,
    RandomBytes = binary_to_list(crypto:strong_rand_bytes(IdLength)),
    lists:map(fun(B) -> lists:nth((B rem length(Charset)) + 1, Charset) end, RandomBytes).
    
