%% @doc whois_data.erl
%%
%% Downloads and prepares the TLDs data
%%
-module(whois_data).

-include_lib("../include/whois_records.hrl").

-export([save_test_data/1]).

-define(TEST_DATA_PATH, <<"test/data">>).

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
            {error, extract_using_re, Data}
    end.

save_test_data(Domain) when is_list(Domain) ->
    save_test_data(list_to_binary(Domain));
save_test_data(Domain) when is_binary(Domain) ->
    {ok, TldRe} = re:compile(?TLD_RE),
    Tld = extract_tld(Domain, [{tld_re, TldRe}]),
    File = <<?TEST_DATA_PATH/binary, "/", Tld/binary, "/", Domain/binary>>,
    Result = whois:lookup(Domain),
    io:format(":~p~n", [Result]),
    %% TODO create ?TEST_DATA_PATH if necessary
    save(File, Result#whois.raw).

save(File, Data) ->
    Dir = filename:dirname(File),
    case file:make_dir(Dir) of
        ok              -> to_file(File, Data);
        {error, eexist} -> to_file(File, Data);
        Error           -> {error, save, File, Error}
    end.

to_file(File, Data) ->
    %% TODO Data is empty
    {ok, Fd} = file:open(File, [raw, write]),
    file:write(Fd, Data),
    file:close(Fd).
