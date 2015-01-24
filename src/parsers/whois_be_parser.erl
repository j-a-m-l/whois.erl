%% TODO

%% @doc 
-module(whois_be_parser).

%% -export([infer_status/1, unavailable/1, available/1, exist/1]).

%% -define(UNAVAILABLE, "TODO").
%% -define(AVAILABLE, "No match for \"").
%% -define(EXIST_RE, <<"^Registered:\\s*(.+)\\s*$">>).
%% 
%% infer_status(Data) ->
%%     [{unavailable, ?UNAVAILABLE},
%%      {available, ?AVAILABLE},
%%      {exist, compile(?EXIST_RE)}].
%% 
%% unavailable(Data) ->
%%     Data.
%% 
%% available(Data) ->
%%     Data.
%% 
%% exist(Data) ->
%%     Result = extract_name(Data),
%%     %% Result = #domain{name = Name},
%%     Result.
