-module(case_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test that generate_id/0 produces a 5-character string.
generate_id_test() ->
    Id = case_server:generate_id(),
    ?assertEqual(5, length(Id)),
    ?assert(lists:all(fun(C) -> lists:member(C, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") end, Id)).

%% Test that get_case_item/1 returns the correct item
get_case_item_test() ->
    {ok, Pid} = case_server:start_link(),
    case_server:edit_case_item("Test Case", title),
    ?assertEqual("Test Case", case_server:get_case_item(title)),
    ?assertEqual("not found", case_server:get_case_item(non_existent_key)),
    gen_server:stop(Pid).

%% Test that start_link/1 correctly starts the server with a predefined case.
start_link_with_case_test() ->
    {ok, Pid} = case_server:start_link({"Test Case", pending, "12345", ["123","456"], ["my_pdf.pdf", "my_text.txt"]}),
    
    %% Ensure the process is alive
    ?assert(erlang:is_process_alive(Pid)),

    % Test if all Case Items are there on start up
    ?assertEqual("Test Case", case_server:get_case_item(title)),
    ?assertEqual(pending, case_server:get_case_item(status)),
    ?assertEqual("12345", case_server:get_case_item(lawyer_id)),
    ?assertEqual(["123","456"], case_server:get_case_item(client_id)),
    ?assertEqual(["my_pdf.pdf", "my_text.txt"], case_server:get_case_item(documents)),
    
    %% Clean up - Stop the GenServer
    gen_server:stop(Pid).

%% Test that start_link/0 correctly starts the server.
start_link_test() ->
    {ok, Pid} = case_server:start_link(),

    %% Ensure the process is alive
    ?assert(erlang:is_process_alive(Pid)),

    % Test if case initialized correctly
    ?assertEqual(undefined, case_server:get_case_item(title)),
    ?assertEqual(pending, case_server:get_case_item(status)),
    ?assertEqual(undefined, case_server:get_case_item(lawyer_id)),
    ?assertEqual([], case_server:get_case_item(client_id)),
    ?assertEqual([], case_server:get_case_item(documents)),

    %% Clean up - Stop the GenServer
    gen_server:stop(Pid).

%% Test that edit_case_item/2 correctly edits an item.
edit_case_item_test() ->
    {ok, Pid} = case_server:start_link(),

    %% Update the title
    case_server:edit_case_item("New Title", title),

    %% Allow some time for the cast operation to be processed
    timer:sleep(50),

    %% Verify the title was updated
    ?assertEqual("New Title", case_server:get_case_item(title)),

    %% Clean up - Stop the GenServer
    gen_server:stop(Pid).

%% Test that clear_case_item/1 correctly clears an item.
%% and that it doesn't clear some items.
clear_case_item_test() ->
    {ok, Pid} = case_server:start_link(),
    case_server:edit_case_item("New Title", title),
    case_server:edit_case_item(["123","456"], client_id),

    %% Ensure the fields exists before removal
    ?assertEqual("New Title", case_server:get_case_item(title)),
    ?assertEqual(["123","456"], case_server:get_case_item(client_id)),

    %% Clear the fields
    case_server:clear_case_item(title),
    case_server:clear_case_item(client_id),


    %% Allow some time for the cast operation to be processed
    timer:sleep(50),

    %% Verify the title and client_id was removed (should return predefined undefined or [])
    ?assertEqual(undefined, case_server:get_case_item(title)),
    ?assertEqual([], case_server:get_case_item(client_id)),

    % get case and attempts to remove timestamp which isn't allowed to
    % be removed. Case is checked to be the same after attempting to remove
    % updated_at
    Old_Case = case_server:get_case(),
    case_server:clear_case_item(updated_at),
    timer:sleep(50),
    ?assertEqual(Old_Case, case_server:get_case()),

    %% Clean up - Stop the GenServer
    gen_server:stop(Pid).

%% Test that get_case/0 correctly returns whole case.
get_case_test() ->
    {ok, Pid} = case_server:start_link(),

    %% Retrieve the full case
    Case = case_server:get_case(),

    % asserts if case is what we want it look like
    ?assertEqual(undefined, maps:get(title, Case)),
    ?assertEqual(pending, maps:get(status, Case)),
    ?assertEqual(undefined, maps:get(lawyer_id, Case)),
    ?assertEqual([], maps:get(client_id, Case)),
    ?assertEqual([], maps:get(documents, Case)),

    %% Clean up - Stop the GenServer
    gen_server:stop(Pid).
    
