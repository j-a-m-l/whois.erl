%% @doc whois_server.erl
%%
%% Default server
%%
-module(whois_server).
-include_lib("../include/tlds.hrl").

-export([start/0, start/1, stop/0]).

%% Is necessary exporting this function for being able to trigger it after spawning a process
-export([init/1]).

-define(DEFAULT_OPS, []).
-define(TIMEOUT, 1000).

%% TODO stricter
-define(DOMAIN_RE, <<"^(?:.+://)?(?:.+\\.)?(\\w+\\.\\w+)(?:/.*)?$">>).
-define(TLD_RE, <<"^.+(\\.\\w+)$">>).

start() ->
    start(?DEFAULT_OPS).
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
    {ok, DomainRe} = re:compile(?DOMAIN_RE),
    {ok, TldRe} = re:compile(?TLD_RE),
    Pid ! started,
    loop([{domain_re, DomainRe}, {tld_re, TldRe}]).

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

loop(Ops) ->
    receive
        {whois, From, stop} ->
            reply(From, ok);
        {whois, From, Query} ->
            reply(From, process(Query, Ops)),
            loop(Ops)
    end.

%% TODO utils module?

extract_domain(Query, Ops) ->
    extract_using_re(Query, Ops, domain_re).

extract_tld(Domain, Ops) ->
    extract_using_re(Domain, Ops, tld_re).

extract_using_re(Data, Ops, Type) ->
    Re = proplists:get_value(Type, Ops),
    {match, [Result]} = re:run(Data, Re, [{capture, [1], binary}]),
    Result.

check_tld(Tld) ->
    case proplists:get_value(Tld, ?TLDS) of
        undefined -> false;
        _ -> true
    end.

get_tld_url(Tld) ->
    get_tld_url(Tld, ?TLDS).
get_tld_url(Tld, [{Tld, Url} | _]) ->
    Url;
get_tld_url(Tld, [_ | Tlds]) ->
    get_tld_url(Tld, Tlds).

%% TODO one process for each query?
process(Query, Ops) ->
    Domain = extract_domain(Query, Ops),
    Tld = extract_tld(Domain, Ops),
    case check_tld(Tld) of
        true ->
            Url = get_tld_url(Tld),
            Response = whois_request:perform(Url, Domain, Ops),
            whois_parser:process(Response, Tld);
        false ->
            %% TODO Reason
            stop()
    end.

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

%% server_test_() ->
%%     [{"The server is started with a registered name at the beginning",
%%       ?setup(fun is_started_registered/0)},
%%      {"The server can be stopped",
%%       ?setup(fun can_stop/0)}].

extract_domain_test_() ->
    {ok, Re} = re:compile(?DOMAIN_RE),
    Ops = [{domain_re, Re}],
    [?_assertEqual(<<"example.com">>, extract_domain(<<"example.com/">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com/index.html">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"www.example.com">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"http://example.com">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com">>, Ops))].

extract_tld_test_() ->
    {ok, Re} = re:compile(?TLD_RE),
    Ops = [{tld_re, Re}],
    [?_assertEqual(<<".org">>, extract_tld(<<"lol.org">>, Ops)),
     ?_assertEqual(<<".com">>, extract_tld(<<"example.com">>, Ops))].

check_tld_test_() ->
    [?_assertEqual(true, check_tld(<<".com">>)),
     ?_assertEqual(false, check_tld(<<"com">>)),
     ?_assertEqual(false, check_tld(<<".l0l">>))].

get_tld_url_test_() ->
    [?_assertEqual(<<"whois.crsnic.net">>, get_tld_url(<<".com">>))].
%% 
%% request_test_() ->
%%     [{"The server performs requests",
%%       ?setup(fun perform_requests/0)},
%%      {"The server parses responses",
%%       ?setup(fun parses/0)}].
%% 
%% is_started_registered() ->
%%     ?assert(erlang:is_process_alive(?MODULE)).
%% 
%% can_stop() ->
%%     ?assert(erlang:is_process_alive(?MODULE)).
%% 
%% perform_requests() ->
%%     [?assertEqual("example.net", ?MODULE:request("Domain Name: EXAMPLE.NET")),
%%      ?assertEqual("example.net", ?MODULE:request("Domain Name: Example.net"))].
%% 
%% parses_responses() ->
%%     [?assertEqual("example.net", ?MODULE:request("Domain Name: EXAMPLE.NET")),
%%      ?assertEqual("example.net", ?MODULE:request("Domain Name: Example.net"))].

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
