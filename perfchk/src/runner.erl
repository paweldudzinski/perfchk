-module(runner).

-behaviour(gen_server).

%% API
-export([start_link/4,
         check_performance/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("print.hrl").
-include("perfchk.hrl").

%% =====================================
%% API
%% =====================================

start_link(Url, SauceUser, BasicAuthHeader, TestName) ->
    gen_server:start_link(?MODULE, [Url, SauceUser, BasicAuthHeader, TestName], []).

check_performance(Pid, ResultHandleMode) ->
    gen_server:cast(Pid, {check_performance, ResultHandleMode}).

%% =====================================
%% Callbacks
%% =====================================

init([Url, SauceUser, BasicAuthHeader, TestName]) ->
    {ok, #auth{url=Url, auth=BasicAuthHeader, user=SauceUser, test_name=TestName}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({check_performance, ResultHandleMode}, #auth{auth=BasicAuthHeader, test_name=TestName} = State) ->
    Result = webdriver:start_session(BasicAuthHeader, TestName),
    check_performance(Result, State, ResultHandleMode),
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

%% =====================================
%% Internals
%% =====================================

check_performance({ok, SessionId}, #auth{url=NavigateToUrl, auth=BasicAuthHeader, user=SauceUser}, ResultHandleMode) ->
    ok = webdriver:url(SessionId, NavigateToUrl),
    ok = webdriver:quit(SessionId),
    JobId = binary_to_list(SessionId),
    ok = wait_until_job_completed(BasicAuthHeader, SauceUser, JobId),
    {ok, Result} = sauce_api:get_metrics(BasicAuthHeader, JobId),
    case ResultHandleMode of
        reply ->
            ?print("~n~n"),
            print_metrics(Result);
        noreply -> ok
    end;
check_performance({error, unauthorized}, _State, _ResultHandleMode) ->
    io:format("Error running performance test, please check your credentials");
check_performance({error, Reason}, _State, _ResultHandleMode) ->
    io:format("Error running performance check with reason ~p~n", [Reason]).


print_metrics(Result) ->
    print_metrics(Result, []).

print_metrics([{<<"detail">>, <<"could not find a job matching selected criteria">>}], _CheckResult) ->
    ?print("We could not find a job matching selected criteria on SauceLabs :(\nPlease try again.~n");
print_metrics([], CheckResult) ->
    print_summary(CheckResult);
print_metrics([{MetricName, Metric0} | Metrics], CheckResult) ->
    {Metric} = lists:nth(length(Metric0), Metric0),
    RealValue = proplists:get_value(<<"r">>, Metric),
    RegimeDetected = proplists:get_value(<<"new_regime">>, Metric) =:= 1,
    print_metric(MetricName, RealValue, RegimeDetected),
    case RegimeDetected of
        true ->
            print_metrics(Metrics, [MetricName | CheckResult]);
        false ->
            print_metrics(Metrics, CheckResult)
    end.

print_metric(MetricName, RealValue, true) ->
    ?print("%r ~p~s~n", humanize_metric(MetricName, RealValue));
print_metric(MetricName, RealValue, false) ->
    ?print("%dg ~p~s~n", humanize_metric(MetricName, RealValue)).

humanize_metric(<<"load">>, Value) ->
    ["Page load       :", Value, " ms"];
humanize_metric(<<"requestsCount">>, Value) ->
    ["Requests        :", Value, ""];
humanize_metric(<<"pageWeight">>, Value) ->
    ["Page weight     :", Value/1000, " KB"];
humanize_metric(<<"speedIndex">>, Value) ->
    ["Speed index     :", list_to_float(float_to_list(Value,[{decimals, 2}])), " ms"];
humanize_metric(Other, Value) ->
    [Other, Value, ""].

humanize_metric_name(<<"load">>) ->
    "page load";
humanize_metric_name(<<"requestsCount">>) ->
    "requests";
humanize_metric_name(<<"pageWeight">>) ->
    "page weight";
humanize_metric_name(<<"speedIndex">>) ->
    "speed index";
humanize_metric_name(MetricName) ->
    binary_to_list(MetricName).

print_summary([]) ->
    ?print("~nPerformance check: $g~n~n", ["pass"]);
print_summary(FailedChecks) ->
    ?print("~nPerformance check: $r~n", ["failed"]),
    ?print("Metrics out of baseline: "),
    lists:foreach(fun(Check) -> ?print("~p ", [humanize_metric_name(Check)]) end, FailedChecks),
    ?print("~n~n").


wait_until_job_completed(BasicAuthHeader, SauceUser, JobId) ->
    wait_until_job_completed(BasicAuthHeader, SauceUser, JobId, 10).

wait_until_job_completed(_BasicAuth, _SauceUser, _JobId, 0) ->
    {error, timeout};
wait_until_job_completed(BasicAuthHeader, SauceUser, JobId, MaxTries) ->
    {ok, Result} = sauce_api:get_job(BasicAuthHeader, SauceUser, JobId),
    case proplists:get_value(<<"status">>, Result) of
        <<"complete">> ->
            timer:sleep(2000),
            ok;
        _Any ->
            ?print("."),
            timer:sleep(2500),
            wait_until_job_completed(BasicAuthHeader, SauceUser, JobId, MaxTries-1)
    end.
