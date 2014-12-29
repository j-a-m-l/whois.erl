%% @doc 
-module(com_parser).
-include_lib("../../include/whois.hrl").

-export([parse/1]).

parse(Data) when is_binary(Data) ->
    io:format(":~p~n", [Data]),
    Data.

unavailable(Data) ->
    Data.

exist(Data) ->
    Data.

available(Data) ->
    Data.

-define(NAME_RE, <<"^\s*Domain Name:\s*([a-z\\d\\-\\.]+)$">>).
extract_name(Data) ->
    {ok, NameRe} = re:compile(?NAME_RE, [caseless]),
    {match, [Name]} = re:run(Data, NameRe, [{capture, [1], binary}]),
    string:to_lower(binary_to_list(Name)).
