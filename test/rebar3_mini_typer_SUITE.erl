%%% @doc Test module for rebar3_mini_typer
-module(rebar3_mini_typer_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([no_files/1]).

all() ->
    [no_files].

no_files(_) ->
    ct:comment("With no files... we get an error"),
    [{abort, <<"typer: no file(s) to analyze">>}] = run_typer(#{}),

    ct:comment("With a non-existing folder... we get an error"),
    [{abort, <<"typer: cannot access /non-existing: No such file or directory">>}] =
        run_typer(#{files_r => ["/non-existing"]}),

    ct:comment("With an empty folder... we get an error"),
    [{abort, <<"typer: no file(s) to analyze">>}] =
        run_typer(#{files_r => [abs_test_path("empty")]}),
    {comment, ""}.

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
