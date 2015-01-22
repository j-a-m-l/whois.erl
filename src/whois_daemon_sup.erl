-module(whois_daemon_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% TODO options
-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Ops) ->
    Procs = [
         {whois_daemon,
              {whois_daemon, start_link, []},
              permanent, 2000, worker, [whois_daemon]}
    ],
	{ok, {{one_for_one, 10, 5}, Procs}}.
