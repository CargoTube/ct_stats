-module(ct_stats).

-export([
         update/0,
         add_message/2
        ]).


update() ->
    ct_stats_messages:update().

add_message(Type, Duration) ->
    ct_stats_messages:add(Type, Duration).
