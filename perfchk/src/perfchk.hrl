-record(auth, {url, auth, user, test_name}).
-record(sauce, {url, user, key, test_name, concurrent_tests}).

-define(ONDEMAND, "https://ondemand.saucelabs.com/wd/hub").
-define(ONDEMAND_SESSION, ?ONDEMAND ++ "/session").
-define(ONDEMAND_URL(SessionId), ?ONDEMAND_SESSION ++ "/" ++ SessionId ++ "/url").
-define(ONDEMAND_QUIT(SessionId), ?ONDEMAND_SESSION ++ "/" ++ SessionId).

-define(SAUCE_API_GET_JOB(SauceUser, JobId), "https://saucelabs.com/rest/v1/" ++ SauceUser ++ "/jobs/" ++ JobId).
-define(SAUCE_API_GET_METRICS(JobId), "https://api.us-west-1.saucelabs.com/v1/performance/metrics/" ++ JobId ++ "/baseline/history/?order_index=0&metric_names=load&metric_names=requestsCount&metric_names=pageWeight&metric_names=speedIndex&limit=50").
