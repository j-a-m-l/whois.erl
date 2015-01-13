%% @doc 
-module(whois_no_parser).

-include_lib("../include/whois.hrl").
-behaviour(whois_parser).
-export([parse/2]).

parse(Domain, Data) ->
    Status = [unavailable],
    Available = case Status of
        [available] -> true;
        _ -> false
    end,
    #whois{name = Domain, status = Status, available = Available}.

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start() -> ok.
stop() -> ok.
-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

-define(TEST_DATA_PATH, "../test/data/").
-define(PARSER, "no").
test_data_for(Domain) ->
    {ok, Data} = file:read_file(list_to_binary([?TEST_DATA_PATH, ?PARSER, "/", Domain])),
    Data.

unavailable_test_() ->
    [{"Returns the domain name",
      ?setup(fun domain_name_test/0)},
     {"Has an 'unavailable' status",
      ?setup(fun status_test/0)},
     {"It's not available",
      ?setup(fun available_test/0)}].

domain_name_test() ->
    Domain = "example2.no",
    Data = test_data_for(Domain),
    Result = parse(Domain, Data),
    [?assertEqual(Domain, Result#whois.name)].

status_test() ->
    Domain = "example2.no",
    Data = test_data_for(Domain),
    Result = parse(Domain, Data),
    [?assertEqual([unavailable], Result#whois.status)].

available_test() ->
    Domain = "example2.no",
    Data = test_data_for(Domain),
    Result = parse(Domain, Data),
    [?assertEqual(false, Result#whois.available)].

%% available_test_() ->
%%     [{"Extracts the domain name",
%%       ?setup(fun parse_domain_name/0)},
%%      {"Extracts the domain name",
%%       ?setup(fun parse_domain_name/0)}].

%% exists_test_() ->
%%     [{"Extracts the domain name",
%%       ?setup(fun parse_domain_name/0)},
%%      {"Extracts the domain name",
%%       ?setup(fun parse_domain_name/0)}].

%% -endif.
