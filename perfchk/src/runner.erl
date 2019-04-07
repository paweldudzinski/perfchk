-module(runner).

-behaviour(gen_server).

-export([start_link/2,
         check_performance/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("perfchk.hrl").

start_link(Url, BasicAuth) ->
    gen_server:start_link(?MODULE, [Url, BasicAuth], []).

check_performance(Pid) ->
    gen_server:cast(Pid, check_performance).

init([Url, BasicAuth]) ->
    {ok, #auth{url=Url, auth=BasicAuth}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(check_performance, #auth{url=Url, auth=BasicAuth} = State) ->
    {ok, SessionId} = webdriver:start_session(BasicAuth),
    ok = webdriver:url(SessionId, Url),
    ok = webdriver:quit(SessionId),
    exit(self(), normal),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
