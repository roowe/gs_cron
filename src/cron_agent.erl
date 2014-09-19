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

%% {test, monthly, [{{1, 1, 1}, {io, fwrite, ["{1, 1, 1} It's running~n"]}}]}
start_link({Name, CycleType, CycleData})->
    gen_server:start_link({local, Name}, ?MODULE, [CycleType, CycleData], []).

get_datetime(Name) ->
    gen_server:call(Name, get_datetime).

cancel(Name) ->
    gen_server:cast(Name, shutdown).

set_datetime(PidOrName, ReferenceGregorianSeconds, CurrentTimeStamp) ->
    gen_server:cast(PidOrName, {set_datetime, ReferenceGregorianSeconds, CurrentTimeStamp}).

is_monthly_day(Day) ->
    Day >= 1 andalso Day =< 31.

is_time(Hour, Minute) ->
    Hour >= 0 andalso Hour < 24 andalso 
        Minute >= 0 andalso Minute < 60.

validate({_, CycleType, CycleData}) ->
    validate(CycleType, CycleData).

validate(monthly, CycleData) ->
    InvalidCycleData = lists:foldl(fun({{DayM, Hour, Minute}, _}=Job, Acc) ->
                                           case {is_monthly_day(DayM), is_time(Hour, Minute)} of
                                               {true, true} ->
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
    State = #state{
               reference_gregorian_seconds = mochiglobal:get(reference_gregorian_seconds),
               current_timestamp = mochiglobal:get(current_timestamp),
               cycle_type = CycleType,
               cycle_data = cycle_data_sort(CycleType, CycleData)
              },
    case validate(State#state.cycle_type,
                  State#state.cycle_data) of
        valid ->
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
%% handle_info(timeout, State = #state{job = {{once, _}, _}}) ->
%%     do_job_run(State, State#state.job),
%%     {stop, normal, State};
handle_info(timeout, State = #state{timeout_type=wait_before_run}) ->
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
cycle_data_sort(monthly, CycleData) ->
    SortCycleData = lists:keysort(1, CycleData),
    ?PRINT("~p~n", [SortCycleData]),
    SortCycleData.


do_job_run(#state{
              mfa = {M, F, A}
             }) ->
    do_job_run({M, F, A});
do_job_run({M, F, A}) ->
    proc_lib:spawn(M, F, A).

%% 返回今天过去的秒数
past_midnight_seconds(State) ->
    calendar:time_to_seconds(element(2, current_datetime(State))).


current_datetime(State) ->
    element(1, current(State)).

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
    try
        {Seconds, MFA} = until_next_time(State),
        ?PRINT("Seconds ~p~n", [Seconds]),
        {ok, Seconds*?MILLISECONDS, MFA}
    catch
        throw:invalid_once_exception ->
            {error, invalid_once_exception}
    end.

normalize_seconds(State, Seconds) ->
    case Seconds - past_midnight_seconds(State) of
        Value when Value >= 0 ->
            Value;
        _ ->
            error_logger:error_msg("invalid_once_exception ~p~n", [erlang:get_stacktrace()]),
            throw(invalid_once_exception)
    end.

until_next_time(#state{
                   cycle_type = monthly,
                   cycle_data = CycleData
                  } = State) ->
    {{{CurrentY, CurrentM, _CurrentD}, _} = CurrentDateTime, GregorianSeconds} = current(State),    
    case monthly_next(CurrentDateTime, GregorianSeconds, 
                      calendar:last_day_of_the_month(CurrentY, CurrentM),
                      CycleData) of
        {Seconds0, MFA0} ->
            {Seconds0, MFA0};
        not_find ->
            monthly_next_month(CurrentDateTime, GregorianSeconds, CycleData)
    end.
    

monthly_next(_, _, _, []) ->
    not_find;
monthly_next({{CurrentY, CurrentM, CurrentD}, _} = CurrentDateTime, 
             GregorianSeconds, LastDay,
             [{{Day, Hour, Min}, MFA}|CycleData]) ->
    if
        Day >= CurrentD andalso 
        Day =< LastDay ->
            NextDateTime = {{CurrentY, CurrentM, Day}, {Hour, Min, 0}},
            NextGregorianSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
            if
                NextGregorianSeconds >= GregorianSeconds ->
                    ?PRINT("Next ~p~n", [NextDateTime]),
                    {NextGregorianSeconds - GregorianSeconds, MFA};
                true ->
                    monthly_next(CurrentDateTime, GregorianSeconds, LastDay, CycleData)
            end;
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


%% @doc Calculates the duration in seconds until the next time this
%% period is to occur during the day.
%% -spec until_next_daytime/2 :: (state(), erlcron:period()) -> erlcron:seconds().
%% until_next_daytime(State, Period) ->
%%     StartTime = first_time(Period),
%%     EndTime = last_time(Period),
%%     case current_time(State) of
%%         T when T > EndTime ->
%%             until_tomorrow(State, StartTime);
%%         T ->
%%             next_time(Period, T) - T
%%     end.

%% @doc Calculates the last time in a given period.
%% -spec last_time/1 :: (erlcron:period()) -> erlcron:seconds().
%% last_time(Period) ->
%%     hd(lists:reverse(lists:sort(resolve_period(Period)))).


%% %% @doc Calculates the first time in a given period.
%% -spec first_time/1 :: (erlcron:period()) -> erlcron:seconds().
%% first_time(Period) ->
%%     hd(lists:sort(resolve_period(Period))).

%% %% @doc Calculates the first time in the given period after the given time.
%% -spec next_time/2 :: (erlcron:period(), erlcron:seconds()) -> erlcron:seconds().
%% next_time(Period, Time) ->
%%     R = lists:sort(resolve_period(Period)),
%%     lists:foldl(fun(X, A) ->
%%                         case X of
%%                             T when T >= Time, T < A ->
%%                                 T;
%%                             _ ->
%%                                 A
%%                         end
%%                 end, 24*3600, R).

%% @doc Returns a list of times given a periodic specification.
%% -spec resolve_period/1 :: ([erlcron:period()] | erlcron:period()) -> [erlcron:seconds()].
%% resolve_period([]) ->
%%     [];
%% resolve_period([H | T]) ->
%%     resolve_period(H) ++ resolve_period(T);
%% resolve_period({every, Duration, {between, TimeA, TimeB}}) ->
%%     Period = resolve_dur(Duration),
%%     StartTime = resolve_time(TimeA),
%%     EndTime = resolve_time(TimeB),
%%     resolve_period0(Period, StartTime, EndTime, []);
%% resolve_period(Time) ->
%%     [resolve_time(Time)].

%% resolve_period0(_, Time, EndTime, Acc) when Time >= EndTime ->
%%     Acc;
%% resolve_period0(Period, Time, EndTime, Acc) ->
%%     resolve_period0(Period, Time + Period, EndTime, [Time | Acc]).

%% %% @doc Returns seconds past midnight for a given time.
%% -spec resolve_time/1 :: (erlcron:cron_time()) -> erlcron:seconds().
%% resolve_time({H, M, S}) when H < 24, M < 60, S < 60  ->
%%     S + M * 60 + H * 3600;
%% resolve_time({H, M, S, X}) when  H < 24, M < 60, S < 60, is_atom(X) ->
%%     resolve_time({H, X}) + M * 60 + S;
%% resolve_time({H, M, X}) when  H < 24, M < 60, is_atom(X) ->
%%     resolve_time({H, X}) + M * 60;
%% resolve_time({12, am}) ->
%%     0;
%% resolve_time({H,  am}) when H < 12 ->
%%     H * 3600;
%% resolve_time({12, pm}) ->
%%     12 * 3600;
%% resolve_time({H,  pm}) when H < 12->
%%     (H + 12) * 3600.

%% %% @doc Returns seconds for a given duration.
%% -spec resolve_dur/1 :: (erlcron:duration()) -> erlcron:seconds().
%% resolve_dur({Hour, hr}) ->
%%     Hour * 3600;
%% resolve_dur({Min, min}) ->
%%     Min * 60;
%% resolve_dur({Sec, sec}) ->
%%     Sec.


%% @doc Calculates the duration in seconds until the given time occurs
%% tomorrow.
%% -spec until_tomorrow/2 :: (state(), erlcron:seconds()) -> erlcron:seconds().
%% until_tomorrow(State, StartTime) ->
%%     (StartTime + 24*3600) - current_time(State).

%% @doc Calculates the duration in seconds until the given period
%% occurs several days from now.
%% until_days_from_now(State, Period, Days) ->
%%     Days * 24 * 3600 + until_next_daytime(State, Period).

%% @doc Calculates the duration in seconds until the given period
%% occurs, which may be today or several days from now.

%% until_next_daytime_or_days_from_now(State, Period, Days) ->
%%     CurrentTime = current_time(State),
%%     case last_time(Period) of
%%         T when T < CurrentTime ->
%%             until_days_from_now(State, Period, Days);
%%         _ ->
%%             until_next_daytime(State, Period)
%%     end.




fast_forward(#state{
                reference_gregorian_seconds = ReferenceGregorianSeconds
               } = State, NewReferenceGregorianSeconds) ->
    {Seconds, MFA} = until_next_time(State),       
    Span = NewReferenceGregorianSeconds - ReferenceGregorianSeconds,
    case Span > Seconds of
        true ->
            %% =:=这个会在返回之后触发，故不在这里处理
            ?PRINT("MFA ~p Seconds ~p, Span ~p~n", [MFA, Seconds, Span]),
            NewState = State#state{reference_gregorian_seconds = ReferenceGregorianSeconds + Seconds + 2},
            do_job_run(MFA),
            fast_forward(NewState, NewReferenceGregorianSeconds);
        false ->
            ok
    end.
