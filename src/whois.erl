%% @doc whois.erl
%% This module acts a as library for looking up WHOIS servers.
-module(whois).

-export([lookup/1, lookup/2]).

%% TODO -spec lookup(binary()) -> {ok, Response}.
lookup(Domain) ->
    lookup(Domain, []).
lookup(Domain, Ops) when is_list(Domain) ->
    lookup(list_to_binary(Domain), Ops);
lookup(Domain, Ops) when is_binary(Domain), is_list(Ops) ->
    whois_server:start_link(Ops),
    Response = whois_server:lookup(Domain),
    whois_server:stop(),
    Response.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
