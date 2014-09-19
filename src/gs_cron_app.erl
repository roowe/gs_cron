-module(gs_cron_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init_datetime(),
    gs_cron_sup:start_link().

stop(_State) ->
    ok.


init_datetime() ->
    mochiglobal:put(reference_gregorian_seconds, calendar:datetime_to_gregorian_seconds(erlang:localtime())),
    mochiglobal:put(current_timestamp, gs_cron_util:unixtime()),
    ok.

