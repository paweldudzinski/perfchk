-module(webdriver_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SESSION_ID, <<"100-100-100">>).
-define(BASIC_AUTH_HEADER, {"Authorization", "Basic TOKEN"}).
-define(TEST_NAME, "Test name").

desired_capabilities_test() ->
    {Caps} = webdriver:desired_capabilities(?TEST_NAME),
    {Details} = proplists:get_value(<<"desiredCapabilities">>, Caps),
    true = proplists:get_value(<<"extendedDebugging">>, Details),
    ?assertEqual(cast:binary(?TEST_NAME) , proplists:get_value(<<"name">>, Details)).

quit_test() ->
    ok = webdriver:quit(?SESSION_ID).

url_test() ->
    meck:new(webdriver, [non_strict]),
    meck:expect(webdriver, url, fun(_SessionId, _Url) -> ok end),
    ok = webdriver:url(?SESSION_ID, "http://www.papaduda.pl"),
    meck:validate(webdriver),
    meck:unload(webdriver).

start_session_unauthorized_test() ->
    {error, Reason} = webdriver:start_session(?BASIC_AUTH_HEADER, ?TEST_NAME),
    ?assertEqual(Reason, unauthorized).

start_session_mocked_auth_test() ->
    meck:new(webdriver, [non_strict]),
    meck:expect(webdriver, start_session, fun(_Auth, _TestName) -> "SessionId" end),
    ?assertEqual("SessionId", webdriver:start_session(?BASIC_AUTH_HEADER, ?TEST_NAME)),
    meck:validate(webdriver),
    meck:unload(webdriver).
