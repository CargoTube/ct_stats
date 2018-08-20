-module(ct_stats_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

init(noparams) ->
    Procs = [
             cts_messages(),
             cts_msg_timer()
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.

cts_messages() ->
    #{ id => messagestat,
       start => {ct_stats_messages, start_link, []}
     }.


cts_msg_timer() ->
    #{ id => msg_timer,
       start => {ct_stats_msg_timer, start_link, []}
     }.
