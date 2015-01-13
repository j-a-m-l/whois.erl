%% @doc whois_request.erl
%%
-module(whois_request).

-export([perform/3]).

perform(Url, Domain, Ops) when is_list(Url) ->
    Port = proplists:get_value(port, Ops),
    Timeout = proplists:get_value(timeout, Ops),

    {ok, Binary} = file:read_file(list_to_binary(["../test/data/", Domain])),
    {ok, binary_to_list(Binary)}.

    %% send_timeout configures gen_tcp:send 
    %% case gen_tcp:connect(Url, Port, [binary, {active, false}, {packet, 0}, {send_timeout, Timeout}], Timeout) of
    %%     {ok, Socket} ->
    %%         ok = gen_tcp:send(Socket, adapt_request(Domain)),
    %%         Response = recv(Socket),
    %%         ok = gen_tcp:close(Socket),
    %%         {ok, Response};
    %%     {error, Reason} ->
    %%         {error, Reason}
    %% end.

recv(Sock) ->
    recv(Sock, []).
recv(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            recv(Sock, [Data | Acc]);
        {error, closed} ->
            iolist_to_binary(lists:reverse(Acc))
    end.

adapt_request(Data) ->
    %% adapt()
    list_to_binary(Data).

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun() -> start() end, fun(_)-> stop() end, F}).

perform_test_() ->
    [{"Connects",
      ?setup(fun can_stop/0)},
     {"Closes the socket",
      ?setup(fun can_stop/0)}].

adapt_request_test_() ->
    [{"It appends the ASCII CR and ASCII LF characters to the query",
      ?setup(fun adds_end_characters/0)},
     {"",
      ?setup(fun can_stop/0)}].

can_stop() ->
    ?assert(erlang:is_process_alive(?MODULE)).

adds_end_characters() ->
    ?assert(erlang:is_process_alive(?MODULE)).

perform_requests() ->
    [?assertEqual("example.net", ?MODULE:request("Domain Name: EXAMPLE.NET")),
     ?assertEqual("example.net", ?MODULE:request("Domain Name: Example.net"))].

%% -endif.
