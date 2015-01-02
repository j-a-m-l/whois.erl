%% @doc whois_verisign.erl
-module(whois_verisign).

adapt_request(Domain) ->
    list_to_binary(["=", Domain, ?REQUEST_END]).
