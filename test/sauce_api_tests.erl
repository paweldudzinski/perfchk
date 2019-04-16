-module(sauce_api_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SAUCE_USER, "erlang-tester").
-define(BASIC_AUTH_HEADER, {"Authorization", "Basic TOKEN"}).
-define(JOB_ID, "testjobid").

get_job_not_found_test() ->
    meck:new(sauce_api, [non_strict]),
    {ok, Result} = sauce_api:get_job(?BASIC_AUTH_HEADER, ?SAUCE_USER, ?JOB_ID),
    ?assertEqual([], Result).

get_job_test_mocked_resp_test() ->
    meck:new(sauce_api, [non_strict]),
    meck:expect(sauce_api, get_job, fun(_Auth, _SauceUser, _JobId) ->
                                        {ok, [{<<"status">>, <<"complete">>}, {<<"id">>, <<"testjobid">>}]}
                                    end),
    {ok, Result} = sauce_api:get_job(?BASIC_AUTH_HEADER, ?SAUCE_USER, ?JOB_ID),
    ?assertEqual(proplists:get_value(<<"id">>, Result), <<"001">>),
    ?assertEqual(proplists:get_value(<<"status">>, Result), <<"complete">>),
    meck:validate(sauce_api),
    meck:unload(sauce_api).
