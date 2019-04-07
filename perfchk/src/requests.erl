-module(requests).

-include("perfchk.hrl").

-export([post/3,
         delete/1,
         get/2]).

post(Url, BasicAuth, Opts) ->
    {ok, {_, _, Body}} = httpc:request(post, {Url, BasicAuth, "application/json", jiffy:encode(Opts)}, [], []),
    {BodyJson} = jiffy:decode(Body),
    {ok, BodyJson}.

delete(SessionId) ->
    Url = ?ONDEMAND_QUIT(binary_to_list(SessionId)),
    {ok, _Result} = httpc:request(delete, {Url, [], "application/json", []}, [], []),
    ok.

get(Url, BasicAuth) ->
    httpc:request(get, {Url, BasicAuth}, [], []).
