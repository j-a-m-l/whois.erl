%% @doc 
-module(whois_parser).

-export([parse/1, parse/2]).

-define(DEFAULT_OPS, []).

parse(Data) ->
    parse(Data, ?DEFAULT_OPS).
parse(Data, Ops) ->
    ok.
