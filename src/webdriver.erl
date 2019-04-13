-module(webdriver).

-export([desired_capabilities/1,
         start_session/2,
         url/2,
         quit/1]).

-include("perfchk.hrl").

start_session(BasicAuthHeader, TestName) ->
    WebDriverUrl = ?ONDEMAND_SESSION,
    case requests:post(WebDriverUrl, BasicAuthHeader, desired_capabilities(TestName)) of
        {ok, Result} ->
            SessionId = proplists:get_value(<<"sessionId">>, Result),
            {ok, SessionId};
        {error, _} = Error ->
            Error
    end.

url(SessionId, NavigateToUrl) ->
    WebDriverUrl = ?ONDEMAND_URL(binary_to_list(SessionId)),
    {ok, _Result} = requests:post(WebDriverUrl, [], {[{<<"url">>, list_to_binary(NavigateToUrl)}]}),
    ok.

quit(SessionId) ->
    WebDriverUrl = ?ONDEMAND_QUIT(binary_to_list(SessionId)),
    ok = requests:delete(WebDriverUrl).

desired_capabilities(TestName) ->
    {[{<<"desiredCapabilities">>,
        {[{<<"browserName">>,<<"chrome">>},
          {<<"platform">>,<<"Windows 10">>},
          {<<"version">>,<<"latest">>},
          {<<"extendedDebugging">>, true},
          {<<"name">>,list_to_binary(TestName)}]}}]}.
