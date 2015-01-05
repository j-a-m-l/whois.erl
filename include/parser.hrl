extract(Data, Re) ->
    %% TODO compile the first time
    {ok, CompiledRe} = re:compile(Re, [caseless, multiline, {newline, any}]),
    {match, [Result]} = re:run(Data, CompiledRe, [{capture, [1], binary}]),
    Result.

compile(Re) ->
    {ok, CompiledRe} = re:compile(Re, [caseless, multiline, {newline, any}]),
    CompiledRe.

match(Data, CompiledRe, Matches)
    {match, [Result]} = re:run(Data, CompiledRe, [{capture, Matches, binary}]),
    Result.

%% TODO
log(Reason) ->
    io:format("logged:~p~n", [Reason]),
    Reason.
