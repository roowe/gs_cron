-module(gs_cron_util).

-export([unixtime/0,
         ceil/1,
         send/2]).

unixtime() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    erlang:trunc((Megasecs * 1000000) + Secs + (Microsecs / 1000000)).

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

send(Pid, Msg) ->    
    Pid ! Msg.


datetime_plus(DateTime, PlusTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) + 
        calendar:time_to_seconds(PlusTime),
    calendar:gregorian_seconds_to_datetime(Seconds).
    
