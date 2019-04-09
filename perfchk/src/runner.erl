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

start_link(Url, SauceUser, BasicAuth, TestName) ->
    gen_server:start_link(?MODULE, [Url, SauceUser, BasicAuth, TestName], []).

check_performance(Pid, ResultHandleMode) ->
    gen_server:cast(Pid, {check_performance, ResultHandleMode}).

%% =====================================
%% Callbacks
%% =====================================

init([Url, SauceUser, BasicAuth, TestName]) ->
    {ok, #auth{url=Url, auth=BasicAuth, user=SauceUser, test_name=TestName}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({check_performance, ResultHandleMode}, #auth{url=NavigateToUrl, auth=BasicAuth, user=SauceUser, test_name=TestName} = State) ->
    {ok, SessionId} = webdriver:start_session(BasicAuth, TestName),
    ok = webdriver:url(SessionId, NavigateToUrl),
    ok = webdriver:quit(SessionId),
    JobId = binary_to_list(SessionId),
    ok = wait_until_job_completed(BasicAuth, SauceUser, JobId),
    {ok, Result} = sauce_api:get_metrics(BasicAuth, JobId),
    case ResultHandleMode of
        reply ->
            ?print("~n"),
            print_metrics(Result);
        noreply -> ok
    end,
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

print_metrics(Result) ->
    print_metrics(Result, []).

print_metrics([{<<"detail">>, <<"could not find a job matching selected criteria">>}], _CheckResult) ->
    ?print("We could not find a job matching selected criteria on SauceLabs :(\nPlease try again.");
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
    ?print("%r ~p~s", humanize_metric(MetricName, RealValue));
print_metric(MetricName, RealValue, false) ->
    ?print("%dg ~p~s", humanize_metric(MetricName, RealValue)).

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
    ?print("~nPerformance check: $g", ["pass"]);
print_summary(FailedChecks) ->
    ?print("~nPerformance check: $r", ["failed"]),
    io:format("Metrics out of baseline: "),
    lists:foreach(fun(Check) -> io:format("~p ", [humanize_metric_name(Check)]) end, FailedChecks),
    ?print("~n").


wait_until_job_completed(BasicAuth, SauceUser, JobId) ->
    wait_until_job_completed(BasicAuth, SauceUser, JobId, 10).

wait_until_job_completed(_BasicAuth, _SauceUser, _JobId, 0) ->
    {error, timeout};
wait_until_job_completed(BasicAuth, SauceUser, JobId, MaxTries) ->
    {ok, Result} = sauce_api:get_job(BasicAuth, SauceUser, JobId),
    case proplists:get_value(<<"status">>, Result) of
        <<"complete">> ->
            timer:sleep(2000),
            ok;
        _Any ->
            io:format("."),
            timer:sleep(2500),
            wait_until_job_completed(BasicAuth, SauceUser, JobId, MaxTries-1)
    end.
