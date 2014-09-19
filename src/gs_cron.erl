-module(gs_cron).

-export([validate/1,
         cron/1,         
         cancel/1,
         datetime/0,
         set_datetime/1]).

validate(Spec) ->
    cron_agent:validate(Spec).

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
