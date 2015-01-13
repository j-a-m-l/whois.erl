%% @doc whois_server.erl
%%
%% Some utils
%%
-module(whois_utils).

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


%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

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

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
