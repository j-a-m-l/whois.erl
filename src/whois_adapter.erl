%% @doc 
-module(whois_adapter).

-callback adapt(Data :: list()) -> list().

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% -endif.
