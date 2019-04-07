-module(runners_manager).

-behaviour(gen_server).

-export([start_link/0, run/0]).

-define(SAUCE_USER_ENV, "SAUCE_USER").
-define(SAUCE_ACCESS_KEY_ENV, "SAUCE_ACCESS_KEY").
-define(CONCURRENT_SESSIONS, 1).

-include("print.hrl").
-include("perfchk.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    Url = get_cli_param(url),
    SauceUser = get_cli_param(u, os:get_env_var(?SAUCE_USER_ENV)),
    SauceAccessKey = get_cli_param(k, os:get_env_var(?SAUCE_ACCESS_KEY_ENV)),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Url, SauceUser, SauceAccessKey], []).

run() ->
    gen_server:cast(?MODULE, run).

init([Url, SauceUser, SauceAccessKey]) ->
    {ok, #sauce{url=Url, user=SauceUser, key=SauceAccessKey}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(run, #sauce{url=Url, user=SauceUser, key=SauceAccessKey} = State) ->
    case lists:any(fun(P) -> is_undefined(P) end, [Url, SauceUser, SauceAccessKey]) of
        true ->
            print_help();
        false ->
            EncodedAuthString = base64:encode_to_string(lists:append([SauceUser,":",SauceAccessKey])),
            BasicAuth = [{"Authorization","Basic " ++ EncodedAuthString}],
            {ok, Processes} = start_concurrent_sessions(Url, BasicAuth, ?CONCURRENT_SESSIONS),
            quit_when_all_done(Processes)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

start_concurrent_sessions(Url, BasicAuth, N) ->
    start_concurrent_sessions(Url, BasicAuth, N, []).

start_concurrent_sessions(_Url, _BasicAuth, 0, Processes) ->
    {ok, Processes};
start_concurrent_sessions(Url, BasicAuth, N, Processes) ->
    {ok, Pid} = runners_sup:start_child(Url, BasicAuth),
    io:format("NEW PROCESS PID: ~p~n", [Pid]),
    runner:check_performance(Pid),
    start_concurrent_sessions(Url, BasicAuth, N-1, [Pid|Processes]).

quit_when_all_done([]) ->
    init:stop(),
    ok;
quit_when_all_done([Process|Porocesses] = All) ->
    case process_info(Process) of
        undefined -> quit_when_all_done(Porocesses);
        _Alive ->
            timer:sleep(200),
            quit_when_all_done(All)
    end.

get_cli_param(Name) ->
    case init:get_argument(Name) of
        {ok,[[Value]]} -> Value;
        _ -> undefined
    end.

get_cli_param(Name, undefined) ->
    get_cli_param(Name);
get_cli_param(Name, false) ->
    get_cli_param(Name);
get_cli_param(Name, Default) ->
    case get_cli_param(Name) of
        undefined -> Default;
        Result -> Result
    end.

is_undefined(Value) ->
    Value =:= undefined.

print_help() ->
    ?print("~n%b~n", ["Erlang PrfChk Usage"]),
    ?print("Check your website performance using your SauceLabs account and compare metrics with tests done in the past.~n"),
    ?print("Command:"),
    ?print("erl -pa _build/default/lib/jiffy/ebin -pa _build/default/lib/perfchk/ebin or ./run.sh~n"),
    ?print("Parameters (all required):"),
    ?print("-u     URL of your website"),
    ?print("-u     Your SauceLabs username"),
    ?print("-k     Your SauceLabs access key~n"),
    ?print("You may skip -u and -k variables if you'd keep this values in env in %dr and %dr~n",
           [?SAUCE_USER_ENV, ?SAUCE_ACCESS_KEY_ENV]).
