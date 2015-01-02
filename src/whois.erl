%% @doc whois.erl
-module(whois).
-include_lib("../include/tlds.hrl").

-export([lookup/1, lookup/2]).

-define(DEFAULT_OPS, [{port, 43}, {timeout, 12000}]).

-define(TEST_DATA_PATH, "../test/data/").

-define(TLD_RE, <<"^.+(\\..+)$">>).

%% -spec lookup(binary()) -> {ok, Response}.
lookup(Domain) ->
    lookup(list_to_binary(Domain), []).
lookup(Domain, Ops) when is_binary(Domain), is_list(Ops) ->
    Tld = extract_tld(Domain),
    Url = get_tld_url(Tld),
    case whois_server:request(binary_to_list(Url), Domain, merge_options(Ops)) of
        {ok, Response} ->
            response(Domain, Response, Ops);
        {error, Reason} ->
            {error, Reason}
    end.

init() ->
    ok = whois_parser:start(),
    {ok, TldRe} = re:compile(?TLD_RE),
    [TldRe].

merge_options(_Ops) ->
    ?DEFAULT_OPS.

%% TODO validate_domain.erl
%% clear_qs(Query) ->

extract_tld(Domain) ->
    [TldRe] = init(),
    {match, [Tld]} = re:run(Domain, TldRe, [{capture, [1], binary}]),
    Tld.

%% check_tld(Tld) ->

get_tld_url(Tld) ->
    get_tld_url(Tld, ?TLDS).
get_tld_url(Tld, [{Tld, Url} | _]) ->
    Url;
get_tld_url(Tld, [_ | Tlds]) ->
    get_tld_url(Tld, Tlds).

response(_Domain, Response, _Ops) ->
    Parsed = whois_parser:parse(Response),
    whois_parser:stop(),
    save(binary_to_list(_Domain), Response),
    io:format("Response: ~s~n", [Response]),
    io:format("Parsed: ~s~n", [Parsed]).

save(File, Data) ->
    {ok, Fd} = file:open([?TEST_DATA_PATH | File], [raw, write]),
    file:write(Fd, Data),
    file:close(Fd). 


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_tld_test_() ->
    [?_assertEqual(<<".org">>, extract_tld(<<"lol.org">>)),
     ?_assertEqual(<<".com">>, extract_tld(<<"example.com">>))].

get_tld_url_test() ->
    ?assertEqual(<<"whois.crsnic.net">>, get_tld_url(<<".com">>)).

-endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
