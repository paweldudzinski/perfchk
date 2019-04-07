-module(perfchk).

-export([start/0]).

start() ->
    application:start(perfchk).
