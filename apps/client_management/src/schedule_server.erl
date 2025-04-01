-module(schedule_server).
-behavior(gen_server).

-export([start_link/0, add_schedule/1, get_reminders/1, delete_schedule/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("schedule.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(schedule_cache, [named_table, public, set]),
    {ok, []}.

add_schedule(Schedule) ->
    gen_server:cast(?MODULE, {add, Schedule}).

get_reminders(UserId) ->
    gen_server:call(?MODULE, {reminders, UserId}).

handle_cast({add, Schedule}, State) ->
    NewState = [Schedule | State],
    ets:delete(schedule_cache, Schedule#schedule.user_id),
    {noreply, NewState};
handle_cast({delete, Id}, State) ->
    NewState = lists:filter(fun(S) -> S#schedule.id =/= Id end, State),
    ets:delete_all_objects(schedule_cache),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({reminders, UserId}, _From, State) ->
    case ets:lookup(schedule_cache, UserId) of
        [{UserId, Cached}] ->
            {reply, Cached, State};
        [] ->
            Reminders = compute_reminders(UserId, State),
            ets:insert(schedule_cache, {UserId, Reminders}),
            {reply, Reminders, State}
    end.

compute_reminders(UserId, State) ->
    Now = calendar:local_time(),
    lists:filter(
        fun(S) ->
            S#schedule.user_id =:= UserId andalso
                is_soon(S#schedule.datetime, Now, S#schedule.reminder_at)
        end,
        State
    ).

is_soon({{Y, M, D}, {H, Min, _}}, {{CY, CM, CD}, {CH, CMin, _}}, RemindMin) ->
    SchedSecs = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, 0}}),
    CurrSecs = calendar:datetime_to_gregorian_seconds({{CY, CM, CD}, {CH, CMin, 0}}),
    RemindTime = SchedSecs - RemindMin * 60,
    CurrSecs >= RemindTime andalso CurrSecs =< SchedSecs.

terminate(_Reason, _State) ->
    io:format("schedule_server stopped~n"),
    ok.
delete_schedule(Id) ->
    gen_server:cast(?MODULE, {delete, Id}).
