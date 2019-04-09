-module(sauce_api).

-export([get_job/3,
         get_metrics/2]).

-include("perfchk.hrl").

get_job(BasicAuth, SauceUser, JobId) ->
    SauceApiUrl = ?SAUCE_API_GET_JOB(SauceUser, JobId),
    {ok, {Result}} = requests:get(SauceApiUrl, BasicAuth),
    {ok, Result}.

get_metrics(BasicAuth, JobId) ->
    SauceApiUrl =  ?SAUCE_API_GET_METRICS(JobId),
    {ok, {Result}} = requests:get(SauceApiUrl, BasicAuth),
    {ok, Result}.
