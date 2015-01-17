%% @doc whois_server.erl
%%
%% Default server
%%
-module(whois_server).
-behaviour(gen_server).

-include_lib("../include/tlds.hrl").

%% Public interface
-export([start_link/1, stop/0, lookup/1, lookup/2]).
%% Development interface
-export([start/1]).
%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

%% TODO stricter
-define(DOMAIN_RE, <<"^(?:.+://)?(?:.+\\.)?(\\w+\\.\\w+)(?:/.*)?$">>).
-define(TLD_RE, <<"^.+\\.(\\w+)$">>).
-define(WHOIS_PORT, 43).
-define(DEFAULT_LOOKUP_TIMEOUT, 10000).

%% Server API

start_link(Ops) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ops, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Server API for development purposes

start(Ops) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Ops, []).

%% Service API

lookup(Query) ->
    lookup(Query, ?DEFAULT_LOOKUP_TIMEOUT).
lookup(Query, Timeout) when is_list(Query) ->
    lookup(list_to_binary(Query), Timeout);
lookup(Query, Timeout) when is_binary(Query), is_integer(Timeout) ->
    gen_server:call(?MODULE, {lookup, Query}, Timeout).

%% TODO looking up with and without '='? {precise}
%% TODO sending formated queries?
%% TODO sending raw queries?

%% gen_server callbacks

init(State) ->
    {ok, DomainRe} = re:compile(?DOMAIN_RE),
    {ok, TldRe} = re:compile(?TLD_RE),
    %% TODO timeout
    {ok, [{timeout, ?DEFAULT_LOOKUP_TIMEOUT} | [{port, ?WHOIS_PORT} | [{domain_re, DomainRe} | [{tld_re, TldRe} | State]]]]}.

handle_call({lookup, Query}, _From, State) ->
    Reply = process_query(Query, State),
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
process_query(Query, State) ->
    case extract_domain(Query, State) of
        error ->
            {error, invalid_domain, Query};
        Domain ->
            search_domain(Domain, State)
    end.

search_domain(Domain, State) ->
    Tld = extract_tld(Domain, State),
    case tld_exists(Tld) of
        true ->

            %% {ok, Binary} = file:read_file(list_to_binary(["../test/data/", Tld, "/", Domain])),
            %% {ok, binary_to_list(Binary)};

            Url = get_tld_url(Tld),
            Adapter = infer_adapter(Tld),
            Data = Adapter:adapt(Domain),
            case request(Url, Data, State) of
                {ok, Response} ->
                    Parser = infer_parser(Tld),
                    Parser:parse(Response);
                {error, Reason} ->
                    {error, request, Reason}
            end;
        false ->
            {error, unknown_tld}
    end.

extract_domain(Query, State) ->
    extract_using_re(Query, State, domain_re).

extract_tld(Domain, State) ->
    extract_using_re(Domain, State, tld_re).

extract_using_re(Data, State, Type) ->
    Re = proplists:get_value(Type, State),
    case re:run(Data, Re, [{capture, [1], binary}]) of
        {match, [Result]} ->
            Result;
        _ ->
            error
    end.

tld_exists(Tld) ->
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

get_tld_adapter(Tld) ->
    get_tld_adapter(Tld, ?TLDS).
get_tld_adapter(Tld, [{Tld, _Url, Adapter} | _]) ->
    Adapter;
get_tld_adapter(Tld, [{Tld, _Url} | _]) ->
    default;
get_tld_adapter(Tld, [_ | Tlds]) ->
    get_tld_adapter(Tld, Tlds).

infer_parser(Tld) ->
    binary_to_atom(<<"whois_", Tld/binary, "_parser">>, latin1).

infer_adapter(Tld) ->
    Adapter = atom_to_binary(get_tld_adapter(Tld), latin1),
    binary_to_atom(<<"whois_", Adapter/binary, "_adapter">>, latin1).

%% save(File, Data) ->
%%     {ok, Fd} = file:open([?TEST_DATA_PATH | File], [raw, write]),
%%     file:write(Fd, Data),
%%     file:close(Fd). 

request(Url, Data, Ops) when is_binary(Url) ->
    request(binary_to_list(Url), Data, Ops).
request(Url, Data, Ops) ->
    Port = proplists:get_value(port, Ops),
    Timeout = proplists:get_value(timeout, Ops),

    %% send_timeout is the option that configures gen_tcp:send timeout
    case gen_tcp:connect(Url, Port, [binary, {active, false}, {packet, 0}, {send_timeout, Timeout}], Timeout) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Data),
            Response = recv(Socket),
            ok = gen_tcp:close(Socket),
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



%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

%% server_test_() ->
%%     [{"The server is started with a registered name at the beginning",
%%       ?setup(fun is_started_registered/0)},
%%      {"The server can be stopped",
%%       ?setup(fun can_stop/0)}].
%% 
%% lookup_test_() ->
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

%% Private API tests

extract_domain_test_() ->
    {ok, Re} = re:compile(?DOMAIN_RE),
    Ops = [{domain_re, Re}],
    [?_assertEqual(error, extract_domain(<<"example">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com/">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com/index.html">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"www.example.com">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"http://example.com">>, Ops)),
     ?_assertEqual(<<"example.com">>, extract_domain(<<"example.com">>, Ops))].

extract_tld_test_() ->
    {ok, Re} = re:compile(?TLD_RE),
    Ops = [{tld_re, Re}],
    [?_assertEqual(error, extract_tld(<<"lol">>, Ops)),
     ?_assertEqual(<<"org">>, extract_tld(<<"lol.org">>, Ops)),
     ?_assertEqual(<<"com">>, extract_tld(<<"example.com">>, Ops))].

tld_exists_test_() ->
    [?_assertEqual(true, tld_exists(<<"com">>)),
     ?_assertEqual(false, tld_exists(<<".com">>)),
     ?_assertEqual(false, tld_exists(<<"l0l">>))].

get_tld_url_test_() ->
    [?_assertEqual(<<"whois.crsnic.net">>, get_tld_url(<<"com">>))].

get_tld_adapter_test_() ->
    [?_assertEqual(default, get_tld_adapter(<<"com">>)),
     ?_assertEqual(verisign, get_tld_adapter(<<"name">>))].

infer_parser_test_() ->
    [?_assertEqual(whois_com_parser, infer_parser(<<"com">>)),
     ?_assertEqual(whois_name_parser, infer_parser(<<"name">>))].

infer_adapter_test_() ->
    [?_assertEqual(whois_default_adapter, infer_adapter(<<"com">>)),
     ?_assertEqual(whois_verisign_adapter, infer_adapter(<<"name">>)),
     ?_assertEqual(whois_eunic_adapter, infer_adapter(<<"ua">>))].

%% lookup_process_query_test_() ->
%%     [{"Connects",
%%       ?setup(fun can_stop/0)},
%%      {"Closes the socket",
%%       ?setup(fun can_stop/0)}].

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
