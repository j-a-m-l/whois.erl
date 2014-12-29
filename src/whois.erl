%% @doc whois.erl
-module(whois).
-include_lib("../include/tlds.hrl").

-export([lookup/1, lookup/2]).

-define(DEFAULT_OPS, [{port, 43}, {timeout, 12000}]).

%% RFC 3912 indicates that the request must end with ASCII CR and ASCII LF
-define(REQUEST_END, <<"\r\n">>).

-define(TEST_DATA_PATH, "../test/data/").

-define(TLD_RE, <<"^.+(\\..+)$">>).

%% TODO
init() ->
    {ok, TldRe} = re:compile(?TLD_RE),
    [TldRe].

%% -spec lookup(binary()) -> {ok, Response}.
lookup(Domain) ->
    lookup(list_to_binary(Domain), []).
lookup(Domain, Ops) when is_binary(Domain), is_list(Ops) ->
    Tld = extract_tld(Domain),
    Url = get_tld_url(Tld),
    case request(binary_to_list(Url), Domain, merge_options(Ops)) of
        {ok, Response} ->
            response(Response, Ops);
        {error, Reason} ->
            {error, Reason}
    end.

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

request(Url, Domain, Ops) when is_list(Url) ->
    Port = proplists:get_value(port, Ops),
    Timeout = proplists:get_value(timeout, Ops),
    %% send_timeout configures gen_tcp:send 
    case gen_tcp:connect(Url, Port, [binary, {active, false}, {packet, 0}, {send_timeout, Timeout}], Timeout) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, list_to_binary([Domain, ?REQUEST_END])),
            Response = recv(Sock),
            ok = gen_tcp:close(Sock),
            save(binary_to_list(Domain), Response),
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end.

recv(Sock) ->
    recv(Sock, []).
recv(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            recv(Sock, [Data | Acc]);
        {error, closed} ->
            iolist_to_binary(lists:reverse(Acc))
    end.

response(Response, _Ops) ->
    io:format("Response: ~s~n", [Response]).

save(File, Data) ->
    {ok, Descriptor} = file:open([?TEST_DATA_PATH | File], [raw, write]),
    file:write(Descriptor, Data),
    file:close(Descriptor). 


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
