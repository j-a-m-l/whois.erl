%% @doc 
%% Adapter for WHOIS requests that converts a domain into a valid request data
-module(whois_default_adapter).

-behaviour(whois_adapter).
-export([adapt/1]).

adapt(Domain) ->
    %% RFC 3912 indicates that the request must end with ASCII CR and ASCII LF
    <<Domain/binary, "\r\n">>.

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

adapt_test_() ->
    [?_assertEqual(<<"example.net\r\n">>, adapt(<<"example.net">>))].

%% -endif.
