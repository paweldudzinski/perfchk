-module(runners_manager_test).
-include_lib("eunit/include/eunit.hrl").

runners_manager_test() ->
    {ok, Pid} = runners_manager:start_link(),
    ?assert(is_pid(Pid)).
