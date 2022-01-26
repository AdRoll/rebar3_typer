%%% @doc Test module for rebar3_mini_typer
-module(rebar3_mini_typer_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([empty/1, single_file/1, annotate/1]).

all() ->
    [empty, single_file, annotate].

empty(_) ->
    ct:comment("With no files... we get an error"),
    [{abort, <<"typer: no file(s) to analyze">>}] = run_typer(#{}),

    ct:comment("With a non-existing folder... we get an error"),
    [{abort, <<"typer: cannot access /non-existing: No such file or directory">>}] =
        run_typer(#{files_r => ["/non-existing"]}),

    ct:comment("With an empty folder... we get an error"),
    [{abort, <<"typer: no file(s) to analyze">>}] =
        run_typer(#{files_r => [abs_test_path("empty")]}),
    {comment, ""}.

single_file(_) ->
    ct:comment("With a single module... we get its types"),
    [{info, <<"\n%% File", _/binary>>},
     {info, <<"%% ----", _/binary>>},
     {info, <<"-spec exported() -> 'ok'.">>},
     {info, <<"-spec not_exported() -> 'ok'.">>}] =
        run_typer(#{files_r => [abs_test_path("single_file")]}),

    ct:comment("With a single module, and mode:show_exported... we get its exported types"),
    [{info, <<"\n%% File", _/binary>>},
     {info, <<"%% ----", _/binary>>},
     {info, <<"-spec exported() -> 'ok'.">>}] =
        run_typer(#{files_r => [abs_test_path("single_file")], mode => show_exported}),
    {comment, ""}.

%% @todo Test annotate_inc_files when https://github.com/erlang/otp/issues/5653 is fixed.
annotate(_) ->
    _ = file:del_dir_r(abs_test_path("annotate/typer_ann")),
    ct:comment("With mode:annotate... only erl files are annotated"),
    [{info, <<"      Processing file: ", _/binary>>},
     {info, <<"             Saved as: \"", PathAndQuote1/binary>>}] =
        run_typer(#{files_r => [abs_test_path("annotate")], mode => annotate}),
    [$\" | ReversedPath1] = lists:reverse(binary_to_list(PathAndQuote1)),
    {ok, Text} =
        file:read_file(
            lists:reverse(ReversedPath1)),
    [<<"exported() -> {'not_exported','included'}.">>,
     <<"not_exported() -> 'not_exported'.">>] =
        [Spec || <<"-spec ", Spec/binary>> <- binary:split(Text, <<"\n">>, [global, trim])],
    {comment, ""}.

%%% PRIVATE FUNCTIONS

run_typer(Opts) ->
    DefaultOpts = #{mode => show, io => default_io()},
    try rebar3_mini_typer:run(
            maps:merge(DefaultOpts, Opts))
    of
        ok ->
            ok
    catch
        error:aborted ->
            ok
    end,
    collect_io().

default_io() ->
    Self = self(),
    Swallow = fun(_Format, _Data) -> swallowed end,
    Send = fun(Level) -> fun(Format, Data) -> Self ! {io, Level, Format, Data} end end,
    SendAndAbort =
        fun(Format, Data) ->
           Self ! {io, abort, Format, Data},
           error(aborted)
        end,
    #{debug => Swallow,
      info => Send(info),
      warn => Send(warn),
      abort => SendAndAbort}.

collect_io() ->
    collect_io([]).

collect_io(Acc) ->
    receive
        {io, Level, Format, Data} ->
            collect_io([{Level, iolist_to_binary(io_lib:format(Format, Data))} | Acc])
    after 250 ->
        lists:reverse(Acc)
    end.

abs_test_path(FilePath) ->
    filename:join(
        code:lib_dir(rebar3_typer), "test/files/" ++ FilePath).
