-module(whois_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_Type, _Args) -> {ok, pid()}.
start(_Type, _Args) ->
	whois_server_sup:start_link().

-spec stop(_State) -> ok.
stop(_State) ->
	ok.
