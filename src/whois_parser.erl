%% @doc 
%% TODO example.com and special domains
-module(whois_parser).
-include_lib("../include/whois.hrl").

-export([process/2]).

process(Tld, Response) ->
    %% Module = list_to_existing_atom(string:concat(Parser, "_parser")),
    Module = list_to_atom(string:concat(Tld, "_parser")),
    Module:parse(Response).


%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

start() -> ok.
stop() -> ok.

process_test_() ->
    [{"Invokes the parser linked to the Tld",
      ?setup(fun tld_parser/0)}]

tld_parser() ->
    [?assertEqual("com_parser", process(".com", <<"Data">>)),
     ?assertEqual("net_parser", process(".net", <<"Data">>))].

%% -endif.
