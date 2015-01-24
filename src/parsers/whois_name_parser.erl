%% TODO

%% @doc 
-module(whois_name_parser).

%% -export([infer_status/1, unavailable/1, available/1, exist/1]).

%% -include_lib("../../include/whois.hrl").

%% -define(PARSER, "com").
%% 
%% -define(UNAVAILABLE, "Not available for registration.").
%% -define(UNAVAILABLE_SECOND_LEVEL, "Not available for second level registration.").
%% -define(AVAILABLE, "No match for \"").
%% -define(EXIST_RE, <<"^\\s*Status:\\s*(.+)\\s*$">>).
%% 
%% infer_status(Data) ->
%%     %% if
%%     %%     string:str(Data, ?UNAVAILABLE) > 0 -> unavailable;
%%     %%     string:str(Data, ?UNAVAILABLE_SECOND_LEVEL) > 0 -> unavailable;
%%     %%     string:str(Data, ?AVAILABLE) > 0 -> available;
%%     %%     {match, _} = re:run(Data, ?EXIST_RE) -> exist;
%%     %%     _ -> unknown
%%     %% end.
%%     {ok, CompiledRe} = re:compile(?EXIST_RE, [caseless, multiline, {newline, any}]),
%%     [{unavailable, ?UNAVAILABLE, ?UNAVAILABLE_SECOND_LEVEL},
%%      {available, ?AVAILABLE},
%%      {exist, CompiledRe}].
%% 
%% unavailable(Data) ->
%%     Data.
%% 
%% available(Data) ->
%%     Data.
%% 
%% exist(Data) ->
%%     Result = extract_name(Data),
%%     %% Result = #domain{name = Name},
%%     Result.
%% 
%% 
%% -define(NAME_RE, <<"^\\s*Domain Name:\\s*([a-z\\d\\-\\.]+)\\s*$">>).
%% extract_name(Data) ->
%%     extract(Data, ?NAME_RE).
%% 
%% -define(REGISTRAR_NAME_RE, <<"^\\s*Registrar:\\s*(.+)\\s*$">>).
%% extract_registrar_name(Data) ->
%%     extract(Data, ?REGISTRAR_NAME_RE).
%% 
%% -define(REGISTRAR_URL_RE, <<"^\\s*Referral URL:\\s*(.+)\\s*$">>).
%% extract_registrar_url(Data) ->
%%     extract(Data, ?REGISTRAR_URL_RE).
%% 
%% %% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl").
%% 
%% -define(TEST_DATA_PATH, "../../test/data/").
%% -define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).
%% 
%% start() -> ok.
%% stop() -> ok.
%% 
%% test_file(File) ->
%%     {ok, Binary} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", File])),
%%     Binary.
%% 
%% infer_unavailable_status_test() ->
%%     [?assertEqual(unavailable, infer_status(test_file("example2.com")))].
%% 
%% 
%% %% parse_test_() ->
%% %%     [{"Accepts strings",
%% %%       ?setup(fun parse_lists/0)},
%% %%      {"Accepts binaries",
%% %%       ?setup(fun parse_lists/0)}].
%% 
%% domain_name_test_() ->
%%     [{"Extracts the domain name",
%%       ?setup(fun parse_domain_name/0)},
%%      {"Extracts the registrar name",
%%       ?setup(fun parse_registrar_name/0)}].
%% 
%% parse_domain_name() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"EXAMPLE2.COM">>, extract_name(Example2))].
%% 
%% parse_registrar_name() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"FABULOUS.COM PTY LDA.">>, extract_registrar_name(Example2))].
%% 
%% parse_registrar_url() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"http://www.fabulous.com">>, extract_registrar_url(Example2))].
%% 
%% parse_name_servers() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual([<<"NS1.MARK.COM">>, <<"NS2.MARK.COM">>], extract_name_servers(Example2))].
%% 
%% parse_status() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"clientTransferProhibited">>, extract_status(Example2))].
%% 
%% parse_creation_date() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"20-oct-2014">>, extract_creation_data(Example2))].
%% 
%% parse_update_date() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"09-nov-2001">>, extract_update_date(Example2))].
%% 
%% parse_expiration_date() ->
%%     {ok, Example2} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", "example2.com"])),
%%     [?assertEqual(<<"09-nov-2015">>, extract_expiration_date(Example2))].
%% 
%% %% -endif.
