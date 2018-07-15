-module(cts_msg_timer).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {count = 0}).


start_link() ->
    gen_server:start_link(?MODULE, no_params, []).

init(no_params) ->
    {ok, #state{}, 100}.

handle_call(_From, _Message, State) ->
    {reply, unsupported, State, 100}.

handle_cast(_Message, State) ->
    {noreply, State, 100}.

handle_info(timeout, #state{count = Count} = State) ->
    cts_messages:tick(),
    maybe_move_msg((Count rem 10 == 0) and (Count > 0) ),
    NewCount = maybe_send_update(Count),
    {noreply, State#state{count = NewCount}, 100};
handle_info(_Message, State) ->
    {noreply, State, 100}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_send_update(3000) ->
    cts_messages:update(),
    0;
maybe_send_update(Count) ->
    Count + 1.

maybe_move_msg(true) ->
    cts_messages:msg_move();
maybe_move_msg(_) ->
    ok.
