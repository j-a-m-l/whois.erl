%% @doc 
%% TODO example.com and special domains
-module(whois_parser).
-include_lib("../include/whois.hrl").

-export([process/1, process/2]).

process(Query) ->
    ?MODULE:process(?MODULE:infer_parser(Query), Query).
process(Parser, Query) ->
    Module = list_to_existing_atom(string:concat(Parser, "_parser")),
    Module:parse(Query).

infer_parser(Query)->
    ok.


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
