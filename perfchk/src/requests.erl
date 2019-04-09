-module(requests).

-include("perfchk.hrl").

-export([post/3,
         delete/1,
         get/2]).

post(Url, BasicAuth, Opts) ->
    case httpc:request(post, {Url, BasicAuth, "application/json", jiffy:encode(Opts)}, [], []) of
        {ok, {{_, 401, "Unauthorized"}, _, _}} ->
            {error, unauthorized};
        {ok, {_, _, Body}} ->
            {BodyJson} = jiffy:decode(Body),
            {ok, BodyJson};
        Any ->
            io:format("Post response: ~p~n", [Any])
    end.

delete(Url) ->
    {ok, _Result} = httpc:request(delete, {Url, [], "application/json", []}, [], []),
    ok.

get(Url, BasicAuth) ->
    {ok, {_, _, Body}} = httpc:request(get, {Url, BasicAuth}, [], []),
    {ok, jiffy:decode(Body)}.
