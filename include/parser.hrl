extract(Data, Re) ->
    %% TODO compile the first time
    {ok, CompiledRe} = re:compile(Re, [caseless, multiline, {newline, any}]),
    {match, [Result]} = re:run(Data, CompiledRe, [{capture, [1], binary}]),
    Result.
