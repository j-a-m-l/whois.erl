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

-define(DEFAULT_LOOKUP_TIMEOUT, 10000).

%% Server API

start_link(Ops) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ops, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Service API

lookup(Query) ->
    lookup(Query, ?DEFAULT_LOOKUP_TIMEOUT);
lookup(Query, Timeout) when is_list(Query) ->
    lookup(list_to_binary(Query), Timeout);
lookup(Query, Timeout) when is_binary(Query) ->
    gen_server:call(?MODULE, {lookup, Query}, Timeout).

%% gen_server callbacks

init(State) ->
    {ok, State}

handle_call({lookup, Query, Ops}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Private API

%% TODO stricter
-define(DOMAIN_RE, <<"^(?:.+://)?(?:.+\\.)?(\\w+\\.\\w+)(?:/.*)?$">>).
-define(TLD_RE, <<"^.+(\\.\\w+)$">>).

%% TODO one process for each query?
process(Query, Ops) ->
    Domain = extract_domain(Query, Ops),
    Tld = extract_tld(Domain, Ops),
    case check_tld(Tld) of
        true ->
            Url = get_tld_url(Tld),
            Response = whois_request:perform(Url, Domain, Ops),
            Parser = infer_parser(Tld),
            Parser:parse(Response);
        false ->
            %% TODO Reason
            stop()
    end.


infer_parser(Tld) ->
    %% list_to_existing_atom(string:concat("whois_", Tld, "_parser")).
    list_to_atom(string:concat("whois_", Tld, "_parser")).
%% 
%% init(Pid) ->
%%     {ok, DomainRe} = re:compile(?DOMAIN_RE),
%%     {ok, TldRe} = re:compile(?TLD_RE),
%%     Pid ! started,
%%     loop([{domain_re, DomainRe}, {tld_re, TldRe}]).

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

%% -endif.


-ifdef(PERF).
%% TODO re vs tokens
%% Tokens = string:tokens(Domain, '.'),

%% TODO load data as binary or string
-endif.
