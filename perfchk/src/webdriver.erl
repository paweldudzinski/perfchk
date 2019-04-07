-module(webdriver).

-behaviour(gen_server).

-export([start_link/0,
         start_session/1,
         url/2,
         quit/1]).

-define(ONDEMAND, "https://ondemand.saucelabs.com/wd/hub").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_session(BasicAuth) ->
    gen_server:call(?MODULE, {start_session, BasicAuth}, infinity).

url(SessionId, Url) ->
    gen_server:call(?MODULE, {url, SessionId, Url}, infinity).

quit(SessionId) ->
    gen_server:call(?MODULE, {quit, SessionId}, infinity).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, undefined}.

handle_call({start_session, BasicAuth}, _From, State) ->
    {ok, Result} = post(?ONDEMAND ++ "/session", BasicAuth, desired_capabilities()),
    SessionId = proplists:get_value(<<"sessionId">>, Result),
    {reply, {ok, SessionId}, State};
handle_call({url, SessionId, Url}, _From, State) ->
    io:format("Handling url call with session: ~p and url: ~p~n", [SessionId, Url]),
    WebDriverUrl = ?ONDEMAND ++ "/session/" ++ binary_to_list(SessionId) ++ "/url",
    io:format("Webdriver url: ~p~n", [WebDriverUrl]),
    {ok, _Result} = post(WebDriverUrl, [], {[{<<"url">>, list_to_binary(Url)}]}),
    {reply, ok, State};
handle_call({quit, SessionId}, _From, State) ->
    io:format("Done session ~p~n", [SessionId]),
    ok = delete(SessionId),
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

desired_capabilities() ->
    {[{<<"desiredCapabilities">>,
        {[{<<"browserName">>,<<"chrome">>},
          {<<"platform">>,<<"Windows 10">>},
          {<<"version">>,<<"latest">>},
          {<<"extendedDebugging">>, true},
          {<<"name">>,<<"PerfChk SL Test">>}]}}]}.

post(Url, BasicAuth, Opts) ->
    {ok, {_, _, Body}} = httpc:request(post, {Url, BasicAuth, "application/json", jiffy:encode(Opts)}, [], []),
    {BodyJson} = jiffy:decode(Body),
    {ok, BodyJson}.

delete(SessionId) ->
    Url = ?ONDEMAND ++ "/session/" ++ binary_to_list(SessionId),
    {ok, _Result} = httpc:request(delete, {Url, [], "application/json", []}, [], []),
    ok.

get(Url, SauceUser, SauceAccessKey) ->
    Encoded = base64:encode_to_string(lists:append([SauceUser,":",SauceAccessKey])),
    Auth = {"Authorization","Basic " ++ Encoded},
    httpc:request(get, {Url, [Auth]}, [], []).
