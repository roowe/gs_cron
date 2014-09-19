-module(cron_agent_sup).

-behaviour(supervisor).

-export([start_link/0,
         add_job/1,
         all_child_pids/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_job(Job) ->
    supervisor:start_child(?SERVER, [Job]).

all_child_pids() ->
    [Child || {_Id, Child, _Type, _Modules} <- supervisor:which_children(?MODULE)].

init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [{undefined,
        {cron_agent, start_link, []},
        transient,
        16#ffffffff,
        worker,
        [cron_agent]
       }
      ]
     }
    }.

