%% @doc 
%% TODO example.com and special domains
-module(whois_parser).
-include_lib("../include/whois.hrl").

-export([process/2]).

process(Data, Tld) when is_list(Data) ->
    process(Tld, list_to_binary(Data));
process(Data, Tld) when is_binary(Data) ->
    %% Module = list_to_existing_atom(string:concat(Tld, "_parser")),
    Module = list_to_atom(string:concat(Tld, "_parser")),
    case Module:infer_status(Data) of
        unavailable -> Module:unavailable(Data);
        available -> Module:available(Data);
        exist -> Module:exist(Data);
        _ -> unknown(Data)
    end.

%% TODO log the data
unknown(Data) ->
    ok.

normalize(Name) ->
    list_to_binary(string:to_lower(binary_to_list(Name))).

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

start() -> ok.
stop() -> ok.

process_test_() ->
    [{"Invokes the parser linked to the Tld",
      ?setup(fun tld_parser/0)}]

    %% {"Normalizes the domain name",
    %%  ?setup(fun normalized_domain_name/0)}].

%% normalized_domain_name() ->
%%     [?assertEqual("example.net", normalize(<<"EXAMPLE.NET">>)),
%%      ?assertEqual("example.net", normalize(<<"Example.net">>))].

tld_parser() ->
    [?assertEqual("com_parser", process(".com", <<"Data">>)),
     ?assertEqual("net_parser", process(".net", <<"Data">>))].

%% -endif.
