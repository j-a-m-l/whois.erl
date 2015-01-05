%% @doc 
-module(whois_br_parser).

-export([status/1]).

-include_lib("../../include/parser.hrl").

status() ->
    Exist = fun(Data) ->
        Re = <<"^TODO:\\s*(.+)\\s*$">>,
        match(Data, compile(Re), [1]);
    end,
    [{unavailable, ["TODO"]},
     {available, ["% No match for domain \""]},
     {exist, Exist}].

process(Data) ->
    Process = fun(Check) ->
        if
            is_list(Check) -> string:str(Data, Check),
            is_function(Check) -> Check(Data),
        end
    end,
    list:map(Process, Checks).
