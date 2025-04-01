-module(scheduler_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, scheduler_sup}, ?MODULE, []).

init([]) ->
    Children = [
        {schedule_server, {schedule_server, start_link, []}, permanent, 5000, worker, [
            schedule_server
        ]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
