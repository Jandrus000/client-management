-module(external_data_sync_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).


start_link()->
    supervisor:start_link({local, external_data_sync_sup}, ?MODULE, []).

init([])->
    Children = 
    [
        {
            fetcher, 
            {fetcher, start_link, [fetcher]},
            permanent,
            5000,
            worker,
            [fetcher]
        }    
        
    ],

    {ok, {{one_for_all, 5, 10}, Children}}.
