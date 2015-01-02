%% @doc whois_server.erl
%%
%% Default server
%%
-module(whois_server).

-export([start/0, stop/0]).

%% Is necessary exporting this function for being able to trigger it after spawning a process
-export([init/1]).

-define(DEFAULT_OPS, []).
-define(TIMEOUT, 1000).

-define(TLD_RE, <<"^.+(\\..+)$">>).

start(_Ops) ->
    register(?MODULE, spawn_link(?MODULE, init, [self()])),
    receive
        started -> ok
    after
        ?TIMEOUT -> {error, starting}
    end.

stop() ->
    call(stop).

init(Pid) ->
    %% {ok, TldRe} = re:compile(?TLD_RE),
    %% [TldRe].
    Pid ! started,
    loop().

call(Request) ->
    Ref = make_ref(),
    ?MODULE ! {whois, {self(), Ref}, Request},
    receive
        {whois_response, Ref, Reply} -> Reply
    after
        ?TIMEOUT -> {error, timeout}
    end.

reply({To, Ref}, Reply) ->
    To ! {whois_response, Ref, Reply}.

loop() ->
    receive
        {whois, From, stop} ->
            reply(From, ok);
        {whois, From, Query} ->
            reply(From, process(Query)),
            loop()
    end.

extract_domain(Query) ->
    {match, [Tld]} = re:run(Domain, TldRe, [{capture, [1], binary}]),
    Tld.

extract_tld(Domain) ->
    {match, [Tld]} = re:run(Domain, TldRe, [{capture, [1], binary}]),
    Tld.

check_tld(Tld) ->
    true.

get_tld_url(Tld) ->
    get_tld_url(Tld, ?TLDS).
get_tld_url(Tld, [{Tld, Url} | _]) ->
    Url;
get_tld_url(Tld, [_ | Tlds]) ->
    get_tld_url(Tld, Tlds).

process(Query) ->
    Domain = extract_domain(Query),
    Tld = extract_tld(Domain),
    if check_tld(Tld)
                true -> Url = get_tld_url(Tld),
                false -> stop()
            end,
    Response = whois_request:perform(Url, Domain, _Ops),
    whois_parser:parse(Response).
%% case whois_server:request(binary_to_list(Url), Domain, merge_options(Ops)) of
%%     {ok, Response} ->
%%         response(Domain, Response, Ops);
%%     {error, Reason} ->
%%         {error, Reason}
%% end.
%% 
%% response(_Domain, Response, _Ops) ->
%%     Parsed = whois_parser:parse(Response),
%%     whois_parser:stop(),
%%     save(binary_to_list(_Domain), Response),
%%     io:format("Response: ~s~n", [Response]),
%%     io:format("Parsed: ~s~n", [Parsed]).
%% 
%% save(File, Data) ->
%%     {ok, Fd} = file:open([?TEST_DATA_PATH | File], [raw, write]),
%%     file:write(Fd, Data),
%%     file:close(Fd). 



%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

server_test_() ->
    [{"The server is started with a registered name at the beginning",
      ?setup(fun is_started_registered/0)},
     {"The server can be stopped",
      ?setup(fun can_stop/0)}].

check_tld_test() ->
    [?assertEqual(true, check_tld(<<".com">>)),
     ?assertEqual(false, check_tld(<<"com">>)),
     ?assertEqual(false, check_tld(<<".l0l">>))].

extract_tld_test_() ->
    [?_assertEqual(<<".org">>, extract_tld(<<"lol.org">>)),
     ?_assertEqual(<<".com">>, extract_tld(<<"example.com">>))].

get_tld_url_test() ->
    ?assertEqual(<<"whois.crsnic.net">>, get_tld_url(<<".com">>)).

request_test_() ->
    [{"The server performs requests",
      ?setup(fun perform_requests/0)},
     {"The server parses responses",
      ?setup(fun parses/0)}].

is_started_registered() ->
    ?assert(erlang:is_process_alive(?MODULE)).

can_stop() ->
    ?assert(erlang:is_process_alive(?MODULE)).

perform_requests() ->
    [?assertEqual("example.net", ?MODULE:request("Domain Name: EXAMPLE.NET")),
     ?assertEqual("example.net", ?MODULE:request("Domain Name: Example.net"))].

parses_responses() ->
    [?assertEqual("example.net", ?MODULE:request("Domain Name: EXAMPLE.NET")),
     ?assertEqual("example.net", ?MODULE:request("Domain Name: Example.net"))].

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
