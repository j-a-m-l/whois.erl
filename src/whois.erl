%% @doc whois.erl
%% This module acts a as library for looking up WHOIS servers.
-module(whois).

-export([lookup/1, lookup/2]).

-define(DEFAULT_OPS, [{port, 43}, {timeout, 12000}]).

%% -spec lookup(binary()) -> {ok, Response}.
lookup(Domain) ->
    lookup(Domain, []).
lookup(Domain, Ops) when is_list(Domain), is_list(Ops) ->
    lookup(list_to_binary(Domain), Ops);
lookup(Domain, Ops) when is_binary(Domain), is_list(Ops) ->
    gen_server:start_link({local, whois_server}, whois_server),
    Pid = start(merge_options(Ops)),
    receive
      {ok, TldRecords} ->
        other:action(TldRecords);
      {error, Reason} ->
        Reason
    end,
    Pid ! {whois, Domain},
    gen_server:cast(whois_server, stop).

%% TODO
merge_options(_Ops) ->
    ?DEFAULT_OPS.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
