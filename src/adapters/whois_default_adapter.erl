%% @doc 
%% Adapter for WHOIS requests that converts a domain into a valid request data
-module(whois_default_adapter).

-behaviour(whois_adapter).
-export([adapt/1]).

%% RFC 3912 indicates that the request must end with ASCII CR and ASCII LF
-define(REQUEST_END, "\r\n").

adapt(Domain) ->
    string:concat(Domain, ?REQUEST_END).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

adapt_test() ->
    [?assertEqual("example.net\r\n", adapt("example.net"))].

-endif.
