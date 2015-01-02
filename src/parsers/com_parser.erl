%% @doc 
-module(com_parser).
-include_lib("../../include/whois.hrl").

-export([parse/1]).

parse(Data) when is_binary(Data) ->
    io:format(":~p~n", [Data]),
    extract_name(Data).

unavailable(Data) ->
    Data.

exist(Data) ->
    Data.

available(Data) ->
    Data.

-define(NAME_RE, <<"\\s*Domain Name:\\s*([a-z\\d\\-\\.]+)\\s*">>).
extract_name(Data) ->
    {ok, NameRe} = re:compile(?NAME_RE, [caseless, multiline]),
    {match, [Name]} = re:run(Data, NameRe, [{capture, [1], binary}]),
    string:to_lower(binary_to_list(Name)).

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DATA_PATH, "../test/data/").
-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

start() -> ok.
stop() -> ok.

%% {ok, Binary} = file:read_file(list_to_binary(["../test/data/", Domain])),
%% {ok, binary_to_list(Binary)}.

domain_name_test_() ->
    [{"Extracts the domain name",
      ?setup(fun parsed_domain_name/0)},
     {"Normalizes the domain name",
      ?setup(fun normalized_domain_name/0)}].

parsed_domain_name() ->
    [?assertEqual("example.net", parse(<<"Domain Name: example.net">>))].

normalized_domain_name() ->
    [?assertEqual("example.net", parse(<<"Domain Name: EXAMPLE.NET">>)),
     ?assertEqual("example.net", parse(<<"Domain Name: Example.net">>))].

%% -endif.
