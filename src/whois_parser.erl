%% @doc 
%% TODO example.com and special domains
-module(whois_parser).
-include_lib("../include/whois.hrl").

-export([start/0, stop/0, parse/1, parse/2]).

%% Is necessary exporting this function for being able to trigger it after spawning a process
-export([init/1]).

-define(DEFAULT_OPS, [{parser, colon}]).
-define(TIMEOUT, 10000).

start() ->
    register(?MODULE, spawn(?MODULE, init, [self()])),
    receive
        started -> ok
    after
        ?TIMEOUT -> {error, starting}
    end.

stop() ->
    call(stop).

parse(Data) ->
    parse(Data, ?DEFAULT_OPS).
parse(Data, Ops) ->
    call({parse, Data, Ops}).

init(Pid) ->
    Pid ! started,
    loop().

call(Request) ->
    Ref = make_ref(),
    ?MODULE ! {request, {self(), Ref}, Request},
    receive
        {reply, Ref, Reply} -> Reply
    after
        ?TIMEOUT -> {error, timeout}
    end.

reply({To, Ref}, Reply) ->
    To ! {reply, Ref, Reply}.

loop() ->
    receive
        {request, From, stop} ->
            reply(From, ok);
        {request, From, {parse, Data, [{parser, Parser}]}} ->
            %% [parser_ | Parser],
            reply(From, extract_name(Data)),
            loop()
    end.

%% .com
-define(NAME_RE, <<"^\s*Domain Name:\s*([a-z\\d\\-\\.]+)$">>).
extract_name(Data) ->
    {ok, NameRe} = re:compile(?NAME_RE, [caseless]),
    {match, [Name]} = re:run(Data, NameRe, [{capture, [1], binary}]),
    string:to_lower(binary_to_list(Name)).



%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

server_test_() ->
    [{"The server is started with a registered name at the beginning",
      ?setup(fun is_started_registered/0)}].

parse_test_() ->
    [{"Extracts the domain name",
      ?setup(fun parsed_domain_name/0)},
     {"Normalizes the domain name",
      ?setup(fun normalized_domain_name/0)}].

is_started_registered() ->
    ?assert(erlang:is_process_alive(?MODULE)).

parsed_domain_name() ->
    [?assertEqual("example.net", ?MODULE:parse("Domain Name: example.net"))].

normalized_domain_name() ->
    [?assertEqual("example.net", ?MODULE:parse("Domain Name: EXAMPLE.NET")),
     ?assertEqual("example.net", ?MODULE:parse("Domain Name: Example.net"))].

%% -endif.
