%%%-------------------------------------------------------------------
%% @doc forex supervisor for workers.
%% @end
%%%-------------------------------------------------------------------

-module(runners_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(M, F, P), {M, {M, F, P}, temporary, 2000, worker, [M]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Url, BasicAuth) ->
    supervisor:start_child(?MODULE, [Url, BasicAuth]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {simple_one_for_one, 0, 1},
           [?CHILD(runner, start_link, [])]}}.

%%====================================================================
%% Internal functions
%%====================================================================
