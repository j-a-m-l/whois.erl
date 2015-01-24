%% @doc whois_data.erl
%%
%% Downloads and prepares the TLDs data
%%
-module(whois_data).

-export([save_test_data/1]).

-define(TEST_DATA_PATH, <<"test">>).

%% TODO copied from whois_server
-define(TLD_RE, <<"^.+\\.(\\w+)$">>).
extract_tld(Domain, State) ->
    extract_using_re(Domain, State, tld_re).
extract_using_re(Data, State, Type) ->
    Re = proplists:get_value(Type, State),
    case re:run(Data, Re, [{capture, [1], binary}]) of
        {match, [Result]} ->
            Result;
        _ ->
            error
    end.

save_test_data(Domain) ->
    {ok, TldRe} = re:compile(?TLD_RE),
    Tld = extract_tld(Domain, [{tld_re, TldRe}]),
    File = <<?TEST_DATA_PATH/binary, "/", Tld/binary, "/", Domain/binary>>,
    Data = whois:lookup(Domain),
    save(File, Data).

save(File, Data) ->
    {ok, Fd} = file:open(File, [raw, write]),
    file:write(Fd, Data),
    file:close(Fd). 
