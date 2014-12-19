%% @doc whois.erl
-module(whois).

-export([lookup/1, lookup/2]).

-define(DEFAULT_OPS, [{port, 43}, {timeout, 12000}]).
-define(TLD_RE, <<"^.+(\\..+)$">>).

%% -record(tld, {
%%         tld,
%%         domain,
%%         subdomains = [],
%%         available = unknown
%%        }).

%% TODO
init() ->
    {ok, TldRe} = re:compile(?TLD_RE),
    [TldRe].

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

load_tlds() ->
    [
        {<<".com">>, <<"whois.crsnic.net">>},
        {<<".org">>, <<"whois.publicinterestregistry.net">>},
        {<<".be">>, <<"whois.dns.be">>}
    ].

get_tld_url(Tld) ->
    get_tld_url(Tld, load_tlds()).
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
            %% It requires \r\n
            ok = gen_tcp:send(Sock, list_to_binary([Domain, <<"\r\n">>])),
            Response = recv(Sock),
            ok = gen_tcp:close(Sock),
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


%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_tld_test() ->
    ?assertEqual(<<".org">>, extract_tld(<<"lol.org">>)),
    ?assertEqual(<<".com">>, extract_tld(<<"example.com">>)).

get_tld_url_test() ->
    ?assertEqual(<<"whois.crsnic.net">>, get_tld_url(<<".org">>)).

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
