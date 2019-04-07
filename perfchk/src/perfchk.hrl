-record(auth, {url, auth}).
-record(sauce, {url, user, key}).

-define(ONDEMAND, "https://ondemand.saucelabs.com/wd/hub").
-define(ONDEMAND_SESSION, ?ONDEMAND ++ "/session").
-define(ONDEMAND_URL(SessionId), ?ONDEMAND_SESSION ++ "/" ++ SessionId ++ "/url").
-define(ONDEMAND_QUIT(SessionId), ?ONDEMAND_SESSION ++ "/" ++ SessionId).
