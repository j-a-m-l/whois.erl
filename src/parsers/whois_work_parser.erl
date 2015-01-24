%% TODO

%% @doc 
-module(whois_work_parser).
-behaviour(whois_parser).

-include_lib("../include/whois_records.hrl").

-export([parse/2]).

parse(Domain, Data) ->
    Status = extract_status(Data),
    Available = case Status of
        available   -> true;
        exists      -> false;
        pending     -> false;
        unavailable -> false;
        _           -> false
    end,
    #whois{domain = Domain, status = Status, available = Available}.

extract_status(Data) ->
    case whois_parser:includes(Data, <<"Status: Not Registered">>) of
        true -> available;
        false ->
            case whois_parser:includes(Data, <<"Status: This domain name is still pending validation and/or contention resolution.">>) of
                true -> pending;
                false ->
                    case whois_parser:includes(Data, <<"TODO TODO">>) of
                        true -> exists;
                        false -> unknown_status
                    end
            end
    end.

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start() -> ok.
stop() -> ok.
-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

-define(EXISTS_EXAMPLE, <<"google">>).
-define(AVAILABLE_EXAMPLE, <<"asdf1234">>).
-define(TEST_DATA_PATH, <<"test/data/">>).
-define(PARSER, <<"work">>).

test_data_for(Domain) when is_list(Domain) ->
    test_data_for(list_to_binary(Domain));
test_data_for(Domain) when is_binary(Domain) ->
    File = filename:absname(<<?TEST_DATA_PATH/binary, ?PARSER/binary, "/", Domain/binary>>),
    case file:read_file(File) of
        {ok, Data} ->
            Data;
        _ ->
            {error, file_not_found, File}
    end.

%% unavailable_test_() ->

available_test_() ->
    Domain = <<?AVAILABLE_EXAMPLE/binary, ".", ?PARSER/binary>>,
    Data = test_data_for(Domain),
    Result = parse(Domain, Data),
    [{"Returns the domain name",
      ?setup( fun() -> domain_test(Domain, Result) end )},
     {"Has an 'available' status",
      ?setup( fun() -> status_test(available, Result) end )},
     {"It's available",
      ?setup( fun() -> available_test(true, Result) end )}].

exists_test_() ->
    Domain = <<?EXISTS_EXAMPLE/binary, ".", ?PARSER/binary>>,
    Data = test_data_for(Domain),
    Result = parse(Domain, Data),
    [{"Returns the domain name",
      ?setup( fun() -> domain_test(Domain, Result) end )},
     {"Has an 'exists' status",
      ?setup( fun() -> status_test(exists, Result) end )},
     {"It's unavailable",
      ?setup( fun() -> available_test(false, Result) end )}].

domain_test(Expected, Result) ->
    [?assertEqual(Expected, Result#whois.domain)].

status_test(Expected, Result) ->
    [?assertEqual(Expected, Result#whois.status)].

available_test(Expected, Result) ->
    [?assertEqual(Expected, Result#whois.available)].

%% -endif.
