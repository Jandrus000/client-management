-module(fetcher).
-behavior(gen_server).

-export([start_link/1, get_users/2, get_events/2, get_documents/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link(Name)->
    gen_server:start_link({local, Name}, ?MODULE, [] ,[]).

get_users(Name, Country)->
    gen_server:call(Name, {get_users, Country}).

% update_user(Name, UserName, NewUser)->
%     gen_server:call(Name, {update_user, {UserName, NewUser}}).

% delete_user(Name, UserName)->
%     gen_server:cast(Name, {delete_user, UserName}).

get_events(Name, Country)->
    gen_server:call(Name, {get_events, Country}).

% schedule_event(Name, NewEvent)->
%     gen_server:cast(Name, {schedule_event, NewEvent}).

% update_event(Name, EvenName, NewEvent)->
%     gen_server:call(Name, {update_event, {EvenName, NewEvent}}).

% delete_event(Name, EvenName)->
%     gen_server:cast(Name, {delete_event, EvenName}).


get_documents(Name, Country)->
    gen_server:call(Name, {get_documents, Country}).

stop(Name)->
    gen_server:call(Name, stop).

init([])->
    {ok, #{usa=>#{users=>["Bob", "Sue", "Jill"], events=> ["Event1", "Event2", "Event3"]}, 
    ukraine=>#{users=>["Sviatoslav", "Victor", "Yaroslav"], events=>["Event4, Event5, Event6"]}}}.

handle_call({get_users, Country}, _From, Data)->
    CountryInfo = maps:get(Country, Data),
    Users = maps:get(users, CountryInfo),
    {reply, Users, Data};

handle_call({get_events, Country}, _From, Data)->
    CountryInfo = maps:get(Country, Data),
    Events = maps:get(events, CountryInfo),
    {reply, Events, Data};

handle_call({get_documents, _Country}, _From, Data)->
    {reply, no_documents, Data};

handle_call(stop, _From, Data)->
    {stop, normal, ok, Data}.

handle_cast(_Reason, Data)->
    {noreply, Data}.

terminate(_Reason, _Data)->
    io:format("Fetcher is Terminated.~n"),
    ok.