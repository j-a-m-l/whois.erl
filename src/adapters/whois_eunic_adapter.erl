%% @doc whois_eunic.erl
%% Adapter for TODO WHOIS request data
-module(whois_eunic_adapter).

-behaviour(whois_adapter).
-export([adapt/1]).

adapt(Domain) ->
    %% RFC 3912 indicates that the request must end with ASCII CR and ASCII LF
    <<"=", Domain/binary, "\r\n">>.

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

adapt_test_() ->
    [?_assertEqual(<<"=example.ua\r\n">>, adapt(<<"example.ua">>))].

%% -endif.
