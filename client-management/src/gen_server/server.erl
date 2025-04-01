-module(server).
-behavior(gen_server).

%% API
-export([start_link/0, add_item/1, remove_item/0, get_items/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link()->
    gen_server:start_link({local, yup}, server, [], []).

add_item(Item)->
    gen_server:cast(yup, {add_item, Item}).

remove_item() ->
    gen_server:cast(yup, remove).

get_items() ->
    gen_server:call(yup, get_items).



init([])->
    {ok, []}.

handle_cast({add_item, Item}, Items) ->
    NewItems = [Item | Items],
    {noreply, NewItems};
handle_cast(remove, Items) ->
    [_|Tail] = Items,
    NewItems = Tail,
    {noreply, NewItems}.

handle_call(get_items, _From, Items) ->
    {reply, Items, Items};
handle_call(stop, _From, Items) ->
    {stop, normal, ok, Items}.

terminate(_Reason, Items)->
    io:format("It stopped ~n~p", [Items]),
    ok.