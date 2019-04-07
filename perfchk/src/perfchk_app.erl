%%%-------------------------------------------------------------------
%% @doc perfchk public API
%% @end
%%%-------------------------------------------------------------------

-module(perfchk_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    {ok, Pid} = perfchk_sup:start_link(),
    runners_manager:run(),
    {ok, Pid}.

start(_StartType, _StartArgs) ->
    start().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
