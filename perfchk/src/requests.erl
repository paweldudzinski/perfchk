-module(requests).

-include("perfchk.hrl").

-export([post/3,
         delete/1,
         get/2]).

-define(HEADERS(Auth), [Auth, {"User-Agent", "Erlang PerfCheck"}]).

post(Url, BasicAuthHeader, Opts) ->
    case httpc:request(post, {Url, ?HEADERS(BasicAuthHeader), "application/json", jiffy:encode(Opts)}, [], []) of
        {ok, {{_, 401, "Unauthorized"}, _, _}} ->
            {error, unauthorized};
        {ok, {_, _, Body}} ->
            {BodyJson} = jiffy:decode(Body),
            {ok, BodyJson};
        Any ->
            Any
    end.

delete(Url) ->
    {ok, _Result} = httpc:request(delete, {Url, [], "application/json", []}, [], []),
    ok.

get(Url, BasicAuthHeader) ->
    {ok, {_, _, Body}} = httpc:request(get, {Url, ?HEADERS(BasicAuthHeader)}, [], []),
    {ok, jiffy:decode(Body)}.
