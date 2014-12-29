%% @doc 
%% TODO example.com and special domains
-module(whois_parser).
-include_lib("../include/whois.hrl").

-export([start/0, stop/0, parse/1, parse/2]).

%% Is necessary exporting this function for being able to trigger it after spawning a process
-export([init/1]).

-define(DEFAULT_OPS, []).
-define(TIMEOUT, 1000).

start() ->
    register(?MODULE, spawn_link(?MODULE, init, [self()])),
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
        {request, From, {parse, Data, _Ops}} ->
            %% TODO
            Parser = "com",
            reply(From, process(Parser, Data)),
            loop()
    end.

process(Parser, Data) ->
    Module = list_to_existing_atom(string:concat(Parser, "_parser")),
    Module:parse(Data).


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
