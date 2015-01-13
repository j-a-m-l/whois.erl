%% @doc 
%% TODO example.com and special domains
-module(whois_parser).
-include_lib("../include/whois.hrl").

-export([process/2]).
%% -export([behaviour_info/1]).

-callback parse(Domain :: list(), Data :: list()) -> list().

%% behaviour_info(callbacks) ->
%%     [{init, 1}, {terminate, 0}].

start() ->
    ok.
start(Ops) ->
    %% init(Ops).
    ok.

stop() ->
    %% terminate().
    ok.

process(Data, Tld) when is_list(Data) ->
    process(Tld, list_to_binary(Data));
process(Data, Tld) when is_binary(Data) ->
    %% Module = list_to_existing_atom(string:concat("whois_", Tld, "_parser")),
    Module = list_to_atom(string:concat("whois_", Tld, "_parser")),
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

extract(Data, Re) ->
    %% TODO compile the first time
    {ok, CompiledRe} = re:compile(Re, [caseless, multiline, {newline, any}]),
    {match, [Result]} = re:run(Data, CompiledRe, [{capture, [1], binary}]),
    Result.

compile(Re) ->
    {ok, CompiledRe} = re:compile(Re, [caseless, multiline, {newline, any}]),
    CompiledRe.

match(Data, CompiledRe, Matches) ->
    {match, [Result]} = re:run(Data, CompiledRe, [{capture, Matches, binary}]),
    Result.

%% TODO
log(Reason) ->
    io:format("logged:~p~n", [Reason]),
    Reason.

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

process_test_() ->
    [{"Invokes the parser linked to the Tld",
      ?setup(fun tld_parser/0)}].

    %% {"Normalizes the domain name",
    %%  ?setup(fun normalized_domain_name/0)}].

%% normalized_domain_name() ->
%%     [?assertEqual("example.net", normalize(<<"EXAMPLE.NET">>)),
%%      ?assertEqual("example.net", normalize(<<"Example.net">>))].

tld_parser() ->
    [?assertEqual("com_parser", process(".com", <<"Data">>)),
     ?assertEqual("net_parser", process(".net", <<"Data">>))].

%% -endif.
