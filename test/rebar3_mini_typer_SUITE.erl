%%% @doc Test module for rebar3_mini_typer
-module(rebar3_mini_typer_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([empty/1, bad_plt/1, single_file/1, annotate/1, annotate_in_place/1]).

all() ->
    [empty, bad_plt, single_file, annotate, annotate_in_place].

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

bad_plt(_) ->
    ct:comment("With an invalid plt.. we get an error"),
    [{abort,
      <<"typer: Dialyzer's PLT is missing or is not up-to-date; please (re)create it">>}] =
        run_typer(#{files_r => [abs_test_path("single_file")], plt => "bad.plt"}),
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
    {ok, FileInfo0} = test_annotate_mode(annotate),
    ct:comment("Even if run twice on a row"),
    %% ensure TypEr deleted and re-wrote the files with the same contents
    %% by running it a second time
    {ok, FileInfo1} = test_annotate_mode(annotate),
    false = FileInfo0 == FileInfo1,
    {comment, ""}.

annotate_in_place(_) ->
    ct:comment("With mode:annotate_in_place... only erl files are directly annotated"),
    {ok, FileInfo0} = test_annotate_mode(annotate_in_place),
    ct:comment("Even if run twice on a row"),
    %% ensure TypEr deleted and re-wrote the files with the same contents
    %% by running it a second time
    {ok, FileInfo1} = test_annotate_mode(annotate_in_place),
    false = FileInfo0 == FileInfo1,
    {comment, ""}.

%%% PRIVATE FUNCTIONS

test_annotate_mode(Mode) ->
    [{info, <<"      Processing file: ", _/binary>>},
     {info, <<"             Saved as: \"", PathAndQuote1/binary>>}] =
        run_typer(#{files_r => [abs_test_path(atom_to_list(Mode))], mode => Mode}),
    [$\" | ReversedPath1] = lists:reverse(binary_to_list(PathAndQuote1)),
    Path1 = lists:reverse(ReversedPath1),
    {ok, Text} = file:read_file(Path1),
    [<<"exported() -> {'not_exported','included'}.">>,
     <<"not_exported() -> 'not_exported'.">>] =
        [Spec || <<"-spec ", Spec/binary>> <- binary:split(Text, <<"\n">>, [global, trim])],
    _ = case Mode of
            annotate ->
                ".ann.erl" = string:find(Path1, ".ann.erl", trailing);
            annotate_in_place ->
                nomatch = string:find(Path1, ".ann.erl", trailing),
                ".erl" = string:find(Path1, ".erl", trailing)
        end,
    file:read_file_info(Path1).

run_typer(Opts) ->
    DefaultOpts =
        #{plt => default_plt(),
          mode => show,
          io => default_io()},
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

%% @doc rebar3 writes the PLT from running dialyzer with the tool to this path,
%%      so there's a high chance we will find a PLT in there.
default_plt() ->
    filename:join([code:lib_dir(rebar3_typer),
                   "..",
                   "..",
                   ["rebar3", "_", rebar_utils:otp_release(), "_plt"]]).

default_io() ->
    Self = self(),
    Swallow =
        fun(Format, Data) ->
           ct:pal(Format, Data),
           swallowed
        end,
    Send = fun(Level) -> fun(Format, Data) -> Self ! {io, Level, Format, Data} end end,
    SendAndAbort =
        fun(Format, Data) ->
           Self ! {io, abort, Format, Data},
           case rand:uniform() of
               -0.1 ->
                   {to, fool, dialyzer};
               _ ->
                   error(aborted)
           end
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
    filename:join([code:lib_dir(rebar3_typer), "test", "files", FilePath]).
