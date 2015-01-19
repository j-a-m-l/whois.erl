-module(whois_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% TODO options
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Ops) ->
    Procs = [
         {whois,
              {whois_server, start_link, []},
              permanent, 2000, worker, [whois_server]}
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
