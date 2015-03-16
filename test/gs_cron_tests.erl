-module(gs_cron_tests).

-include_lib("eunit/include/eunit.hrl").
-define(MFA, {io, fwrite, [" It's running~n"]}).

%%%===================================================================
%%% Types
%%%===================================================================
cron_test_() ->
    {setup,
     fun() ->
             application:start(gs_cron)
     end,
     fun(_) ->
             application:stop(gs_cron)
     end,
     {with, [
             fun monthly_cron_test/1,
             fun weekly_cron_test/1,
             fun hourly_cron_test/1,
             fun daily_cron_test/1,
             fun once_cron_test/1,
             fun validation_test/1]}}.

%% Time jumps ahead one day so we should see the alarms from both days.
monthly_cron_test(_) ->    
    receive_clean(),
    gs_cron:set_datetime({{2014,9,15}, {8,0,0}}),

    Self = self(),

    gs_cron:cron({monthly_cron_test, monthly, 
                  [{{20,9,10}, {gs_cron_util, send, [Self, monthly_cron_test_ack1]}},
                   {{20,9,11}, {gs_cron_util, send, [Self, monthly_cron_test_ack2]}}]}),
    gs_cron:set_datetime({{2014,9,20}, {9, 10, 58}}),
    ?assertMatch(ok, receive
                         monthly_cron_test_ack1 -> ok
                     after
                         1500 -> timeout
                     end),

    ?assertMatch(ok, receive
                         monthly_cron_test_ack2 -> ok
                     after
                         2500 -> timeout
                     end).

weekly_cron_test(_) ->
%% September 2014
%% Su Mo Tu We Th Fr Sa
%%     1  2  3  4  5  6
%%  7  8  9 10 11 12 13
%% 14 15 16 17 18 19 20
%% 21 22 23 24 25 26 27
%% 28 29 30
    receive_clean(),
    gs_cron:set_datetime({{2014,9,17}, {0, 0, 59}}),
    Self = self(),

    gs_cron:cron({weekly_cron_test, weekly, 
                  [{{3,0,1}, {gs_cron_util, send, [Self, weekly_cron_test_ack1]}},
                   {{7,9,10}, {gs_cron_util, send, [Self, weekly_cron_test_ack2]}}]}),
    ?assertMatch(ok, receive
                         weekly_cron_test_ack1-> ok
                     after
                         1500 -> timeout
                     end),
    gs_cron:set_datetime({{2014,9,21}, {9, 9, 58}}),
    ?assertMatch(ok, receive
                         weekly_cron_test_ack2 -> ok
                     after
                         2500 -> timeout
                     end).

once_cron_test(_) ->
    receive_clean(),
    gs_cron:set_datetime({{2014,9,22}, {0, 0, 58}}),
    Self = self(),

    gs_cron:cron({once_cron_test, once, 
                  [{{2014,9,22, 0, 1, 0}, {gs_cron_util, send, [Self, once_cron_test_ack1]}},
                   {{2014,9,22, 0, 2, 59}, {gs_cron_util, send, [Self, once_cron_test_ack2]}}]}),
    ?assertMatch(ok, receive
                         once_cron_test_ack1-> ok
                     after
                         2500 -> timeout
                     end),
    gs_cron:set_datetime({{2014,9,22}, {0, 3, 0}}),
    ?assertMatch(ok, receive
                         once_cron_test_ack2 -> ok
                     after
                         1500 -> timeout
                     end).

hourly_cron_test(_) ->
    receive_clean(),
    Self = self(),
    gs_cron:set_datetime({{2015, 3, 2}, {0, 0, 0}}),
    gs_cron:cron({hourly_cron_test,
                  hourly,
                  [{{0, 1},
                    {gs_cron_util, send, [Self, hourly_cron_test_ack1]}},
                   {{30, 0},
                    {gs_cron_util, send, [Self, hourly_cron_test_ack2]}}]}),
    

    ?assertMatch(ok, receive
                         hourly_cron_test_ack1 -> ok
                     after
                         1200 -> timeout
                     end),

    
    gs_cron:set_datetime({{2015, 3, 2}, {0, 29, 59}}),
    ?assertMatch(ok, receive
                         hourly_cron_test_ack2 -> ok
                     after
                         1200 -> timeout
                     end).

daily_cron_test(_) ->
    receive_clean(),
    gs_cron:set_datetime({{2014,9,22}, {0, 0, 59}}),
    Self = self(),

    gs_cron:cron({daily_cron_test, daily, 
                  [{{0,1,1}, {gs_cron_util, send, [Self, daily_cron_test_ack1]}},
                   {{9,10}, {gs_cron_util, send, [Self, daily_cron_test_ack2]}}]}),
    ?assertMatch(ok, receive
                         daily_cron_test_ack1-> ok
                     after
                         2300 -> timeout
                     end),
    gs_cron:set_datetime({{2014,9,22}, {9, 9, 58}}),
    ?assertMatch(ok, receive
                         daily_cron_test_ack2 -> ok
                     after
                         2500 -> timeout
                     end).


validation_test(_) ->
    ?assertMatch(valid, gs_cron:validate({test, monthly, 
                                          [{{1, 1, 1}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, monthly, 
                                          [{{31, 23, 59}, ?MFA}, {{1, 23, 59}, ?MFA}]
                                         })),
    ?assertMatch(invalid, gs_cron:validate({test, monthly, 
                                            [{{33, 1, 1}, ?MFA}]
                                           })),
    ?assertMatch(invalid, gs_cron:validate({test, monthly, 
                                            [{{31, 80, 59}, ?MFA}]
                                           })),
    

    ?assertMatch(valid, gs_cron:validate({test, weekly, 
                                          [{{1, 1, 1}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, weekly, 
                                          [{{7, 23, 59}, ?MFA}, {{2, 23, 59}, ?MFA}]
                                         })),
    ?assertMatch(invalid, gs_cron:validate({test, weekly, 
                                            [{{33, 1, 1}, ?MFA}]
                                           })),
    ?assertMatch(invalid, gs_cron:validate({test, weekly, 
                                            [{{31, 80, 59}, ?MFA}]
                                           })),

    ?assertMatch(valid, gs_cron:validate({test, daily, 
                                          [{{1, 0}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, daily, 
                                          [{{23, 59}, ?MFA}]
                                         })),
    ?assertMatch(invalid, gs_cron:validate({test, daily, 
                                            [{{1, 60}, ?MFA}]
                                           })),
    ?assertMatch(invalid, gs_cron:validate({test, daily, 
                                            [{{80, 59}, ?MFA}]
                                           })),
    
    ?assertMatch(valid, gs_cron:validate({test, daily, 
                                          [{{1, 0, 1}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, daily, 
                                          [{{23, 59, 1}, ?MFA}]
                                         })),
    ?assertMatch(invalid, gs_cron:validate({test, daily, 
                                            [{{1, 60, 30}, ?MFA}]
                                           })),
    ?assertMatch(invalid, gs_cron:validate({test, daily, 
                                            [{{1, 59, 90}, ?MFA}]
                                           })),
    
    ?assertMatch(valid, gs_cron:validate({test, hourly, 
                                          [{{59, 59}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, hourly, 
                                          [{{0, 29}, ?MFA}]
                                         })),
    ?assertMatch(invalid, gs_cron:validate({test, hourly, 
                                            [{{60, 30}, ?MFA}]
                                           })),
    ?assertMatch(invalid, gs_cron:validate({test, hourly, 
                                            [{{59, 60}, ?MFA}]
                                           })),

    ?assertMatch(valid, gs_cron:validate({test, once, 
                                          [{{2012,11,1,10,0,0}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, once, 
                                          [{{2012,12,1,10,0,0}, ?MFA}]
                                         })),
    ?assertMatch(valid, gs_cron:validate({test, once, 
                                            [{{2012,12,1,10,0,0}, ?MFA},{{2013,12,1,10,0,0}, ?MFA}]
                                           })),
    ?assertMatch(invalid, gs_cron:validate({test, once, 
                                            [{{2013,2,29,10,0,0}, ?MFA}]
                                           })).
        


receive_clean() ->
    receive
        _ ->
            receive_clean()
    after 
        0 ->
            ok
    end.
