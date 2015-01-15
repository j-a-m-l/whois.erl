%% @doc whois_server.erl
%%
%% Default server
%%
-module(whois_server).
-behaviour(gen_server).

-include_lib("../include/tlds.hrl").

%% Public interface
-export([start_link/1, stop/0, lookup/1, lookup/2]).
%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

%% TODO stricter
-define(DOMAIN_RE, <<"^(?:.+://)?(?:.+\\.)?(\\w+\\.\\w+)(?:/.*)?$">>).
-define(TLD_RE, <<"^.+(\\.\\w+)$">>).
-define(DEFAULT_LOOKUP_TIMEOUT, 10000).

%% Server API

start_link(Ops) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ops, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Service API

lookup(Query) ->
    lookup(Query, ?DEFAULT_LOOKUP_TIMEOUT).
lookup(Query, Timeout) when is_list(Query) ->
    lookup(list_to_binary(Query), Timeout);
lookup(Query, Timeout) when is_binary(Query), is_integer(Timeout) ->
    gen_server:call(?MODULE, {lookup, Query}, Timeout).

%% gen_server callbacks

init(State) ->
    {ok, DomainRe} = re:compile(?DOMAIN_RE),
    {ok, TldRe} = re:compile(?TLD_RE),
    {ok, [{domain_re, DomainRe} | [{tld_re, TldRe} | State]]}.

handle_call({lookup, Query}, _From, State) ->
    Reply = process(Query, State),
    io:format("Reply:~p~n", [Reply]),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% TODO
%% code_change() ->

%% Private API

%% TODO one process for each query?
process(Query, State) ->
    Domain = extract_domain(Query, State),
    Tld = extract_tld(Domain, State),
    case check_tld(Tld) of
        true ->
            Url = get_tld_url(Tld),
            Response = request(Url, Domain, State),
            Parser = infer_parser(Tld),
            Parser:parse(Response);
        false ->
            %% TODO Reason
            stop()
    end.

request(Url, Domain, Ops) when is_list(Url) ->
    Port = proplists:get_value(port, Ops),
    Timeout = proplists:get_value(timeout, Ops),

    {ok, Binary} = file:read_file(list_to_binary(["../test/data/", Domain])),
    {ok, binary_to_list(Binary)}.

    %% send_timeout configures gen_tcp:send 
    %% case gen_tcp:connect(Url, Port, [binary, {active, false}, {packet, 0}, {send_timeout, Timeout}], Timeout) of
    %%     {ok, Socket} ->
    %%         ok = gen_tcp:send(Socket, adapt_request(Domain)),
    %%         Response = recv(Socket),
    %%         ok = gen_tcp:close(Socket),
    %%         {ok, Response};
    %%     {error, Reason} ->
    %%         {error, Reason}
    %% end.

recv(Sock) ->
    recv(Sock, []).
recv(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            recv(Sock, [Data | Acc]);
        {error, closed} ->
            iolist_to_binary(lists:reverse(Acc))
    end.

adapt_request(Data) ->
    %% adapt()
    list_to_binary(Data).

extract_domain(Query, State) ->
    extract_using_re(Query, State, domain_re).

extract_tld(Domain, State) ->
    extract_using_re(Domain, State, tld_re).

extract_using_re(Data, State, Type) ->
    Re = proplists:get_value(Type, State),
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

infer_parser(Tld) ->
    %% list_to_existing_atom(string:concat("whois_", Tld, "_parser")).
    list_to_atom(string:concat("whois_", Tld, "_parser")).

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

%% extract_domain_test_() ->
%%     {ok, Re} = re:compile(?DOMAIN_RE),
%%     Ops = [{domain_re, Re}],
%%     [?_assertEqual(<<"example.com">>, extract_domain(<<"example.com/">>, Ops)),
%%      ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com/index.html">>, Ops)),
%%      ?_assertEqual(<<"example.com">>, extract_domain(<<"www.example.com">>, Ops)),
%%      ?_assertEqual(<<"example.com">>, extract_domain(<<"http://example.com">>, Ops)),
%%      ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com">>, Ops))].
%% 
%% extract_tld_test_() ->
%%     {ok, Re} = re:compile(?TLD_RE),
%%     Ops = [{tld_re, Re}],
%%     [?_assertEqual(<<".org">>, extract_tld(<<"lol.org">>, Ops)),
%%      ?_assertEqual(<<".com">>, extract_tld(<<"example.com">>, Ops))].
%% 
%% check_tld_test_() ->
%%     [?_assertEqual(true, check_tld(<<".com">>)),
%%      ?_assertEqual(false, check_tld(<<"com">>)),
%%      ?_assertEqual(false, check_tld(<<".l0l">>))].
%% 
%% get_tld_url_test_() ->
%%     [?_assertEqual(<<"whois.crsnic.net">>, get_tld_url(<<".com">>))].
%% 
%% perform_test_() ->
%%     [{"Connects",
%%       ?setup(fun can_stop/0)},
%%      {"Closes the socket",
%%       ?setup(fun can_stop/0)}].
%% 
%% adapt_request_test_() ->
%%     [{"It appends the ASCII CR and ASCII LF characters to the query",
%%       ?setup(fun adds_end_characters/0)},
%%      {"",
%%       ?setup(fun can_stop/0)}].
%% 
%% can_stop() ->
%%     ?assert(erlang:is_process_alive(?MODULE)).
%% 
%% adds_end_characters() ->
%%     ?assert(erlang:is_process_alive(?MODULE)).
%% 
%% perform_requests() ->
%%     [?assertEqual("example.net", ?MODULE:request("Domain Name: EXAMPLE.NET")),
%%      ?assertEqual("example.net", ?MODULE:request("Domain Name: Example.net"))].

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
