%%%-------------------------------------------------------------------
%% @doc perfchk top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(perfchk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(M, F, P, T), {M, {M, F, P}, temporary, 2000, T, [M]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Runner = ?CHILD(runners_manager, start_link, [], worker),
    RunnersSupervisor = ?CHILD(runners_sup, start_link, [], supervisor),
    WebDriver = ?CHILD(webdriver, start_link, [], worker),
    Strategy = {one_for_one, 5, 10},
    Processes = [RunnersSupervisor, Runner, WebDriver],
    {ok, {Strategy, lists:flatten(Processes)}}.
