-module(cron_agent).

-behaviour(gen_server).

%% API
-export([start_link/1,
         cancel/1,
         get_datetime/1,
         set_datetime/3,
         validate/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("internal.hrl").

-record(state, {
          mfa,
          cycle_type,
          cycle_data,
          reference_gregorian_seconds,
          current_timestamp,
          fast_forward = false,
          timeout_type
         }).

-define(MILLISECONDS, 1000).
-define(WAIT_BEFORE_RUN, 2000).

start_link({Name, CycleType, CycleData})->
    gen_server:start_link({local, Name}, ?MODULE, [CycleType, CycleData], []).

get_datetime(Name) ->
    gen_server:call(Name, get_datetime).

cancel(Name) ->
    gen_server:cast(Name, shutdown).

set_datetime(PidOrName, ReferenceGregorianSeconds, CurrentTimeStamp) ->
    gen_server:cast(PidOrName, {set_datetime, ReferenceGregorianSeconds, CurrentTimeStamp}).

is_mfa({_, _, _}) ->
    true;
is_mfa(_) ->
    false.

is_monthly_day(Day) ->
    Day >= 1 andalso Day =< 31.

is_weekly_day(Day) ->
    Day >= 1 andalso Day =< 7.

is_time(Hour, Minute) ->
    Hour >= 0 andalso Hour < 24 andalso 
        Minute >= 0 andalso Minute < 60.

is_time(Hour, Minute, Seconds) ->
    Hour >= 0 andalso Hour < 24 andalso 
        Minute >= 0 andalso Minute < 60 andalso
        Seconds >=0 andalso Seconds < 60.

validate({_, CycleType, CycleData}) ->
    validate(CycleType, CycleData).

validate(once, CycleData) ->
    validate2(CycleData, fun({{Year, Month, Day, Hour, Minute, Seconds}, MFA}) ->
                                 calendar:valid_date({Year, Month, Day}) andalso is_time(Hour, Minute, Seconds) andalso is_mfa(MFA)
                         end);
validate(daily, CycleData) ->
    validate2(CycleData, fun({{Hour, Minute}, MFA}) ->
                                 is_time(Hour, Minute) andalso is_mfa(MFA)
                         end);
validate(weekly, CycleData) ->
    validate2(CycleData, fun({{DayW, Hour, Minute}, MFA}) ->
                                 is_weekly_day(DayW) andalso 
                                     is_time(Hour, Minute) andalso is_mfa(MFA)
                         end);
validate(monthly, CycleData) ->
    validate2(CycleData, fun({{DayM, Hour, Minute}, MFA}) ->
                                 is_monthly_day(DayM) andalso 
                                     is_time(Hour, Minute) andalso is_mfa(MFA)
                         end).


validate2(CycleData, Fun) ->
    InvalidCycleData = lists:foldl(fun(Job, Acc) ->
                                           case Fun(Job) of
                                               true ->
                                                   Acc;
                                               _ ->
                                                   [Job|Acc]
                                           end
                                   end, [], CycleData),
    if
        InvalidCycleData =:=[] ->
            valid;
        true ->
            ?ERROR("InvalidCycleData ~p~n", [InvalidCycleData]),
            invalid
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([CycleType, CycleData]) 
  when is_tuple(CycleData) ->
    init([CycleType, [CycleData]]);
init([CycleType, CycleData]) ->
    case validate(CycleType, CycleData) of
        valid ->
            State = #state{
                       reference_gregorian_seconds = mochiglobal:get(reference_gregorian_seconds),
                       current_timestamp = mochiglobal:get(current_timestamp),
                       cycle_type = CycleType,
                       cycle_data = cycle_data_sort(CycleType, CycleData)
                      },
            case until_next_milliseconds(State) of
                {ok, Millis, MFA} ->
                    {ok, State#state{
                           mfa = MFA
                          }, Millis};
                {error, _}  ->
                    {stop, normal}
            end;
        invalid ->
            {stop, normal}
    end.

%% @private
handle_call(_Msg, _From, State) ->
    case until_next_milliseconds(State) of
        {ok, Millis} ->
            {reply, ok, State, Millis};
        {error, _}  ->
            {stop, normal, ok, State}
    end.

%% @private
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast({set_datetime, ReferenceGregorianSeconds, CurrentTimeStamp}, State) ->
    %% 第一次要计算reference_gregorian_seconds下，后面就省去计算了
    fast_forward(State#state{
                   fast_forward=true,
                   reference_gregorian_seconds = element(2, current(State))
                  }, ReferenceGregorianSeconds),
    NewState = State#state{
                 reference_gregorian_seconds = ReferenceGregorianSeconds,
                 current_timestamp = CurrentTimeStamp
                },
    case until_next_milliseconds(NewState) of
        {ok, Millis, MFA} ->
            {noreply, NewState#state{
                        mfa = MFA
                       }, Millis};
        {error, _}  ->
            {stop, normal, NewState}
    end.
%% @private
handle_info(timeout, #state{
                        timeout_type = wait_before_run
                       } = State) ->
    NewState = State#state{timeout_type=normal},
    case until_next_milliseconds(NewState) of
        {ok, Millis, MFA} ->
            {noreply, NewState#state{
                        mfa = MFA
                       }, Millis};
        {error, _}  ->
            {stop, normal, NewState}
    end;
handle_info(timeout, State) ->
    do_job_run(State),
    NewState = State#state{timeout_type=wait_before_run},
    {noreply, NewState, ?WAIT_BEFORE_RUN}.


%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cycle_data_sort(once, [{{_, _, _, _, _, _}, _}|_]=CycleData0) ->
    CycleData = [begin
                     GregorianSeconds = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Seconds}}),
                     {GregorianSeconds, MFA}
                 end || {{Year, Month, Day, Hour, Minute, Seconds}, MFA} <- CycleData0],
    cycle_data_sort(once, CycleData);
cycle_data_sort(_, CycleData) ->
    SortCycleData = lists:keysort(1, CycleData),
    ?PRINT("~p~n", [SortCycleData]),
    SortCycleData.


do_job_run(#state{
              mfa = {M, F, A}
             }) ->
    do_job_run({M, F, A});
do_job_run({M, F, A}) ->
    proc_lib:spawn(M, F, A).


current(#state{
           fast_forward = true,
           reference_gregorian_seconds = ReferenceGregorianSeconds
          }) ->
    {calendar:gregorian_seconds_to_datetime(ReferenceGregorianSeconds),
     ReferenceGregorianSeconds};
current(#state{
           reference_gregorian_seconds = ReferenceGregorianSeconds,
           current_timestamp = CurrentTimeStamp
          }) ->
    Elapsed = gs_cron_util:unixtime() - CurrentTimeStamp,
    NewReferenceGregorianSeconds = Elapsed + ReferenceGregorianSeconds,
    {calendar:gregorian_seconds_to_datetime(NewReferenceGregorianSeconds),
     NewReferenceGregorianSeconds}.


until_next_milliseconds(State) ->
    case until_next_time(State) of
        not_found ->
            {error, not_found};
        {Seconds, MFA} ->
            ?PRINT("Seconds ~p~n", [Seconds]),
            {ok, Seconds*?MILLISECONDS, MFA}
    end.


until_next_time(#state{
                   cycle_type = once,
                   cycle_data = CycleData
                  } = State) ->    
    {_CurrentDateTime, GregorianSeconds} = current(State),
    case [{NextGregorianSeconds, MFA} || {NextGregorianSeconds, MFA} <- CycleData, 
                                         NextGregorianSeconds >= GregorianSeconds] of
        [] ->
            not_found;
        [{NextGregorianSeconds, MFA}|_] ->
            Seconds = NextGregorianSeconds - GregorianSeconds,
            %% NextDateTime = calendar:gregorian_seconds_to_datetime(NextGregorianSeconds),
            %% ?PRINT("Next ~p~n", [NextDateTime]),
            {Seconds, MFA}
    end;
until_next_time(#state{
                   cycle_type = daily,
                   cycle_data = CycleData
                  } = State) ->
    {CurrentDateTime, GregorianSeconds} = current(State),
    case daily_next(CurrentDateTime, GregorianSeconds, CycleData) of
        {Seconds, MFA} ->
            {Seconds, MFA};
        not_found ->
            daily_next_day(CurrentDateTime, GregorianSeconds, CycleData)
    end;
until_next_time(#state{
                   cycle_type = weekly,
                   cycle_data = CycleData
                  } = State) ->
    {{CurrentDate, _} = CurrentDateTime, GregorianSeconds} = current(State),    
    WeekDay = calendar:day_of_the_week(CurrentDate),
    case weekly_next(CurrentDateTime, GregorianSeconds, WeekDay, CycleData) of
        {Seconds, MFA} ->
            {Seconds, MFA};
        not_found ->
            weekly_next_week(CurrentDateTime, GregorianSeconds, WeekDay, CycleData)
    end;
until_next_time(#state{
                   cycle_type = monthly,
                   cycle_data = CycleData
                  } = State) ->
    {{{CurrentY, CurrentM, _CurrentD}, _} = CurrentDateTime, GregorianSeconds} = current(State),    
    case monthly_next(CurrentDateTime, GregorianSeconds, 
                      calendar:last_day_of_the_month(CurrentY, CurrentM),
                      CycleData) of
        {Seconds, MFA} ->
            {Seconds, MFA};
        not_found ->
            monthly_next_month(CurrentDateTime, GregorianSeconds, CycleData)
    end.


daily_next(_, _, []) ->
    not_found;
daily_next({CurrentDate, _} = CurrentDateTime, GregorianSeconds,
           [{{Hour, Min}, MFA}|CycleData]) ->
    NextDateTime = {CurrentDate, {Hour, Min, 0}},
    if        
        NextDateTime >= CurrentDateTime ->
            NextGregorianSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
            ?PRINT("Next ~p~n", [NextDateTime]),
            {NextGregorianSeconds - GregorianSeconds, MFA};
        true ->
            daily_next(CurrentDateTime, GregorianSeconds, CycleData)
    end.

daily_next_day({CurrentDate, _}, GregorianSeconds,
               [{{Hour, Min}, MFA}|_]) ->
    NextGregorianSeconds = calendar:datetime_to_gregorian_seconds({CurrentDate, {0, 0, 0}}) + calendar:time_to_seconds({24 + Hour, Min, 0}),    
    %% NextDateTime = calendar:gregorian_seconds_to_datetime(NextGregorianSeconds),
    %% ?PRINT("Next ~p~n", [NextDateTime]),
    {NextGregorianSeconds - GregorianSeconds, MFA}.


weekly_next(_, _, _, []) ->
    not_found;
weekly_next({CurrentDate, _} = CurrentDateTime, 
             GregorianSeconds, CurrentWeekDay,
             [{{DayW, Hour, Min}, MFA}|CycleData]) ->
    DayDiff = DayW - CurrentWeekDay,
    NextGregorianSeconds = calendar:datetime_to_gregorian_seconds({CurrentDate, {0, 0, 0}}) + calendar:time_to_seconds({DayDiff*24 + Hour, Min, 0}),
    if
        NextGregorianSeconds >= GregorianSeconds ->
            %% NextDateTime = calendar:gregorian_seconds_to_datetime(NextGregorianSeconds),
            %% ?PRINT("Next ~p~n", [NextDateTime]),
            {NextGregorianSeconds - GregorianSeconds, MFA};
        true ->
            weekly_next(CurrentDateTime, GregorianSeconds, CurrentWeekDay, CycleData)
    end.

weekly_next_week({CurrentDate, _}, 
                   GregorianSeconds, CurrentWeekDay,
                   [{{DayW, Hour, Min}, MFA}|_]) ->
    DayDiff = DayW - CurrentWeekDay + 7,
    NextGregorianSeconds = calendar:datetime_to_gregorian_seconds({CurrentDate, {0, 0, 0}}) + calendar:time_to_seconds({DayDiff*24 + Hour, Min, 0}),    
    %% NextDateTime = calendar:gregorian_seconds_to_datetime(NextGregorianSeconds),
    %% ?PRINT("Next ~p~n", [NextDateTime]),
    {NextGregorianSeconds - GregorianSeconds, MFA}.
       

monthly_next(_, _, _, []) ->
    not_found;
monthly_next({{CurrentY, CurrentM, CurrentD}, _} = CurrentDateTime, 
             GregorianSeconds, LastDay,
             [{{Day, Hour, Min}, MFA}|CycleData]) ->
    NextDateTime = {{CurrentY, CurrentM, Day}, {Hour, Min, 0}},
    if
        Day >= CurrentD andalso 
        Day =< LastDay andalso
        NextDateTime >= CurrentDateTime ->
            NextGregorianSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
            ?PRINT("Next ~p~n", [NextDateTime]),
            {NextGregorianSeconds - GregorianSeconds, MFA};
        true ->
            monthly_next(CurrentDateTime, GregorianSeconds, LastDay, CycleData)
    end.
                  
monthly_next_month({{CurrentY, CurrentM, _CurrentD}, _}, 
                   GregorianSeconds,
                   [{{Day, Hour, Min}, MFA}|_]) ->
    NextDateTime = {next_month(CurrentY, CurrentM, Day), {Hour, Min, 0}},
    NextGregorianSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
    ?PRINT("Next ~p~n", [NextDateTime]),
    {NextGregorianSeconds - GregorianSeconds, MFA}.
            
next_month(Year, Month, Day) ->
    {NextYear, NextMonth} =
        case Month of
            12 ->
                {Year + 1, 1};
            _  ->
                {Year, Month + 1}
        end,
    Last = calendar:last_day_of_the_month(NextYear, NextMonth),
    if
        Last < Day ->
            next_month(NextYear, NextMonth, Day);
        true ->
            {NextYear, NextMonth, Day}
    end.

fast_forward(#state{
                cycle_type = CycleType,
                reference_gregorian_seconds = ReferenceGregorianSeconds
               } = State, NewReferenceGregorianSeconds) ->
    case until_next_time(State) of
        not_found ->
            ok;
        {Seconds, MFA} ->
            {Seconds, MFA} = until_next_time(State),       
            Span = NewReferenceGregorianSeconds - ReferenceGregorianSeconds,
            case Span > Seconds of
                true ->
                    %% =:=这个会在返回之后触发，故不在这里处理
                    ?PRINT("~p MFA ~p Seconds ~p, Span ~p~n", [CycleType, MFA, Seconds, Span]),
                    NewState = State#state{reference_gregorian_seconds = ReferenceGregorianSeconds + Seconds + 2},
                    do_job_run(MFA),
                    fast_forward(NewState, NewReferenceGregorianSeconds);
                false ->
                    ok
            end
    end.

    
