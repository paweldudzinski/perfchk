-module(webdriver).

-behaviour(gen_server).

-export([start_link/0,
         start_session/2,
         url/2,
         quit/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("perfchk.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_session(BasicAuth, TestName) ->
    gen_server:call(?MODULE, {start_session, BasicAuth, TestName}, infinity).

url(SessionId, NavigateToUrl) ->
    gen_server:call(?MODULE, {url, SessionId, NavigateToUrl}, infinity).

quit(SessionId) ->
    gen_server:call(?MODULE, {quit, SessionId}, infinity).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, undefined}.

handle_call({start_session, BasicAuth, TestName}, _From, State) ->
    WebDriverUrl = ?ONDEMAND_SESSION,
    {ok, Result} = requests:post(WebDriverUrl, BasicAuth, desired_capabilities(TestName)),
    SessionId = proplists:get_value(<<"sessionId">>, Result),
    {reply, {ok, SessionId}, State};
handle_call({url, SessionId, NavigateToUrl}, _From, State) ->
    WebDriverUrl = ?ONDEMAND_URL(binary_to_list(SessionId)),
    {ok, _Result} = requests:post(WebDriverUrl, [], {[{<<"url">>, list_to_binary(NavigateToUrl)}]}),
    {reply, ok, State};
handle_call({quit, SessionId}, _From, State) ->
    WebDriverUrl = ?ONDEMAND_QUIT(SessionId),
    ok = requests:delete(WebDriverUrl),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    inets:stop(),
    ssl:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

desired_capabilities(TestName) ->
    {[{<<"desiredCapabilities">>,
        {[{<<"browserName">>,<<"chrome">>},
          {<<"platform">>,<<"Windows 10">>},
          {<<"version">>,<<"latest">>},
          {<<"extendedDebugging">>, true},
          {<<"name">>,list_to_binary(TestName)}]}}]}.
