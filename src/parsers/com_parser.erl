%% @doc 
-module(com_parser).
-include_lib("../../include/whois.hrl").

-export([parse/1]).

-define(PARSER, "com").

parse(Data) when is_list(Data) ->
    parse(list_to_binary(Data));
parse(Data) when is_binary(Data) ->
    extract_name(Data).

%% unavailable(Data) ->
%%     Data.
%% 
%% exist(Data) ->
%%     Data.
%% 
%% available(Data) ->
%%     Data.

-define(NAME_RE, <<"^\\s*Domain Name:\\s*([a-z\\d\\-\\.]+)\\s*$">>).
extract_name(Data) ->
    extract(Data, ?NAME_RE).

-define(REGISTRAR_NAME_RE, <<"^\\s*Registrar:\\s*(.+)\\s*$">>).
extract_registrar_name(Data) ->
    extract(Data, ?REGISTRAR_NAME_RE).

-define(REGISTRAR_URL_RE, <<"^\\s*Referral URL:\\s*(.+)\\s*$">>).
extract_registrar_url(Data) ->
    extract(Data, ?REGISTRAR_URL_RE).

extract(Data, Re) ->
    %% TODO compile only the first time
    {ok, CompiledRe} = re:compile(Re, [caseless, multiline, {newline, any}]),
    {match, [Result]} = re:run(Data, CompiledRe, [{capture, [1], binary}]),
    Result.

normalize(Name) ->
    list_to_binary(string:to_lower(binary_to_list(Name))).

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DATA_PATH, "../../test/data/").
-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

start() -> ok.
stop() -> ok.

%% parse_test_() ->
%%     [{"Accepts strings",
%%       ?setup(fun parse_lists/0)},
%%      {"Accepts binaries",
%%       ?setup(fun parse_lists/0)}].

domain_name_test_() ->
    [{"Extracts the domain name",
      ?setup(fun parse_domain_name/0)},
     {"Extracts the registrar name",
      ?setup(fun parse_registrar_name/0)}].

     %% {"Normalizes the domain name",
     %%  ?setup(fun normalized_domain_name/0)}].

parse_domain_name() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"EXAMPLE2.COM">>, extract_name(Example2))].

parse_registrar_name() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"FABULOUS.COM PTY LDA.">>, extract_registrar_name(Example2))].

parse_registrar_url() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"http://www.fabulous.com">>, extract_registrar_url(Example2))].

parse_name_servers() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual([<<"NS1.MARK.COM">>, <<"NS2.MARK.COM">>], extract_name_servers(Example2))].

parse_status() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"clientTransferProhibited">>, extract_status(Example2))].

parse_creation_date() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"20-oct-2014">>, extract_creation_data(Example2))].

parse_update_date() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"09-nov-2001">>, extract_update_date(Example2))].

parse_expiration_date() ->
    {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
    [?assertEqual(<<"09-nov-2015">>, extract_expiration_date(Example2))].

%% normalized_domain_name() ->
%%     [?assertEqual("example.net", normalize(<<"EXAMPLE.NET">>)),
%%      ?assertEqual("example.net", normalize(<<"Example.net">>))].

%% -endif.
