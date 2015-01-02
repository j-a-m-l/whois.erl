%% @doc whois_server.erl
%%
%% Default server
%%
-module(whois_server).

-export([request/3]).

%% RFC 3912 indicates that the request must end with ASCII CR and ASCII LF
-define(REQUEST_END, <<"\r\n">>).

request(Url, Domain, Ops) when is_list(Url) ->
    Port = proplists:get_value(port, Ops),
    Timeout = proplists:get_value(timeout, Ops),

    {ok, Binary} = file:read_file(list_to_binary(["../test/data/", Domain])),
    {ok, binary_to_list(Binary)}.

    %% send_timeout configures gen_tcp:send 
    %% case gen_tcp:connect(Url, Port, [binary, {active, false}, {packet, 0}, {send_timeout, Timeout}], Timeout) of
    %%     {ok, Sock} ->
    %%         ok = gen_tcp:send(Sock, adapt_request(Domain)),
    %%         Response = recv(Sock),
    %%         ok = gen_tcp:close(Sock),
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

adapt_request(Domain) ->
    list_to_binary([Domain, ?REQUEST_END]).
