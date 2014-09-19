-module(gs_cron).

-export([validate/1,
         cron/1,         
         cancel/1,
         datetime/0,
         set_datetime/1]).

-export([test/0]).


validate(Spec) ->
    cron_agent:validate(Spec).

test() ->
    application:stop(?MODULE),
    application:start(?MODULE),
    cron({test, monthly, gen_test()}).

gen_test() ->
    [{{Day, random:uniform(24)-1, random:uniform(60)-1}, {io, fwrite, [integer_to_list(Day) ++ " It's running~n"]}} || Day <- lists:seq(20, 30, 3) ++ lists:seq(2, 19, 3)].


cron(Job) ->
    cron_agent_sup:add_job(Job).

cancel(Name) ->
    cron_agent:cancel(Name).

datetime() ->
    calendar:gregorian_seconds_to_datetime(mochiglobal:get(reference_gregorian_seconds)).

set_datetime(DateTime) ->
    CurrentTimeStamp = gs_cron_util:unixtime(),
    ReferenceGregorianSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    mochiglobal:put(reference_gregorian_seconds, ReferenceGregorianSeconds),
    mochiglobal:put(current_timestamp, CurrentTimeStamp),
    [cron_agent:set_datetime(Pid, ReferenceGregorianSeconds, CurrentTimeStamp) || Pid <- cron_agent_sup:all_child_pids()].
