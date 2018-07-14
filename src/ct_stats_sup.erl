-module(ct_stats_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

init(noparams) ->
    Procs = [
             #{ id => messagestat,
                start => {cts_messages, start_link, []}
              }
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.
