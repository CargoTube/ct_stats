-module(cts_messages).

-behaviour(gen_server).

-export([add/2,
         update/0,
         tick/0,
         msg_move/0,

         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
          entries = [],
          count = 0,
          counts = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
          msg_sec = [],
          avg_msg_sec = 0,

          fastest = 0,
          percentile25 = 0,
          percentile50 = 0,
          percentile75 = 0,
          percentile99 = 0,
          slowest = 0
         }).


add(Type, Duration) ->
    gen_server:cast(?MODULE, {add, Type, Duration/1000.0}).

update() ->
    gen_server:cast(?MODULE, update).

tick() ->
    gen_server:cast(?MODULE, tick).

msg_move() ->
    gen_server:cast(?MODULE, msg_move).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_params, []).

init(no_params) ->
    MaxMsgSec = application:get_env(ct_stats, max_msg_keep, 3600),
    TimeMs = application:get_env(ct_router, stat_update, 300000),
    timer:apply_interval(TimeMs, ?MODULE, update, []),
    {ok, #state{msg_sec = create_list(MaxMsgSec, [])}}.

handle_call(_From, _Message, State) ->
    {reply, unsupported, State}.

handle_cast({add, Type, Duration}, #state{entries = Entries,
                                          count = Count} = State) ->
    Now = erlang:system_time(second),
    NewEntries = [ { Duration, Type, Now} | Entries ],
    NewCount = Count + 1,
    {noreply, State#state{entries = NewEntries, count = NewCount} };
handle_cast(update, State) ->
    {noreply, perform_update(State)};
handle_cast(tick, State) ->
    {noreply, perform_tick(State)};
handle_cast(msg_move, State) ->
    {noreply, perform_msg_move(State)};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({updated, Map}, State) ->
    {noreply, update_with_map(State, Map)};
handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


perform_update(#state{entries = []} = State) ->
    State;
perform_update(#state{entries = Entries} = State) ->
    UpdateFun = fun() ->
                        calculate_percentile(Entries, self())
                end,
    spawn(UpdateFun),
    State.

perform_tick(#state{count = Count, counts = Counts} = State) ->
    NewCounts = [ Count | lists:droplast(Counts)],
    State#state{count = 0, counts = NewCounts}.

perform_msg_move(#state{msg_sec = MsgSecs, counts = Counts} = State) ->
    Sum = lists:sum(Counts),
    NewMsgSecs = [ Sum | lists:droplast(MsgSecs) ],
    State#state{msg_sec = NewMsgSecs}.

calculate_percentile(Entries, Pid) ->
    Now = erlang:system_time(second),
    MaxKeepSec = application:get_env(ct_router, stat_keep, 86400),
    StillOkayTime = Now - MaxKeepSec,

    FilterOld =
        fun({_Duration, _Type, Time} = Entry, {List, Oldest}) ->
                case {Time >= StillOkayTime, Time < Oldest} of
                    {true, true}  ->
                        {[ Entry | List], Time};
                    {true, false} ->
                        {[ Entry | List], Oldest};
                    _ ->
                        {List, Oldest}
                end
        end,


    {ActiveEntries, Oldest}  = lists:foldl(FilterOld, {[], Now}, Entries),
    Sort =
        fun({DurationA, _, _}, {DurationB, _, _}) ->
                DurationA =< DurationB
         end,
    Sorted = lists:sort(Sort, ActiveEntries),
    Length = length(Sorted),
    Percentile25 = percentile( 0.25,  Sorted, Length),
    Percentile50 = percentile( 0.5,  Sorted, Length),
    Percentile75 = percentile( 0.75,  Sorted, Length),
    Percentile99 = percentile( 0.99,  Sorted, Length),
    {Fastest, _, _} = lists:nth(1, Sorted),
    {Slowest, _, _} = lists:nth(Length, Sorted),
    AvgMsgSec = Length / (Now - Oldest),


    Pid ! {updated, #{
             fastest => Fastest,
             percentile25 => Percentile25,
             percentile50 => Percentile50,
             percentile75 => Percentile75,
             percentile99 => Percentile99,
             slowest => Slowest,
             avg_msg_sec => AvgMsgSec
            }}.

update_with_map(#{fastest := Fastest, percentile25 := Percentile25,
                  percentile50 := Percentile50, percentile75 := Percentile75,
                  percentile99 := Percentile99, slowest := Slowest,
                  avg_msg_sec := AvgMsgSec}, State) ->
    lager:info("stats: ~p msg/sec [ ~p / ~p / ~p / ~p / * ~p * / ~p ] ms",
               [AvgMsgSec, Fastest, Percentile25, Percentile50, Percentile75,
                Percentile99, Slowest]),
    State#state{ fastest = Fastest,
                 percentile25 = Percentile25,
                 percentile50 = Percentile50,
                 percentile75 = Percentile75,
                 percentile99 = Percentile99,
                 slowest = Slowest,
                 avg_msg_sec = AvgMsgSec}.



percentile(Percentil, Entries, Length) ->
    Lower = 1.0/(Length + 1.0),
    Upper = Length/(Length + 1.0),
    Position = percentile_pos(Percentil, Length, Lower, Upper),
    FPos = math:floor(Position),
    calc_percentile(FPos, Position - FPos, Entries).

percentile_pos(Percentil, _, Lower, _) when Percentil =< Lower ->
    1;
percentile_pos(Percentil, Length, _, Upper) when Percentil >= Upper ->
    Length;
percentile_pos(Percentil, Length, _ ,_) ->
    Percentil * Length.

calc_percentile(Pos, 0.0, Entries) when is_integer(Pos) ->
    {Result, _, _} = lists:nth(Pos, Entries),
    Result;
calc_percentile(Pos, Partial, Entries) when is_integer(Pos) ->
    {A, _, _} = lists:nth(Pos, Entries),
    {B, _, _} = lists:nth(Pos + 1, Entries),
    A + (B - A) * Partial;
calc_percentile(Pos, Partial, Entries) when is_float(Pos) ->
    calc_percentile(round(Pos), Partial, Entries).


create_list(0, List) ->
    List;
create_list(Count, List) ->
    create_list(Count -1, [ 0 |  List ]).
