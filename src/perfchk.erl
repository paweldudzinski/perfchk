-module(perfchk).

-export([start/0]).

start() ->
    inets:start(),
    ssl:start(),
    application:start(perfchk).
