%%% @doc Plugin provider for rebar3 rebar3_typer.
-module(rebar3_typer_prv).

-export([init/1, do/1, format_error/1]).

-ignore_xref([do/1,
              format_error/1,
              {providers, create, 1},
              {rebar_state, add_provider, 2},
              {rebar_state, command_parsed_args, 1}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, typer},
                          {module, ?MODULE},
                          {bare, true}, % The task can be run by the user, always true
                          {deps, []},
                          {example, "rebar3 typer"},
                          {opts, opts()},
                          {short_desc, "Execute TypEr on your code"},
                          {desc, "Execute TypEr on your code"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, {rebar3_typer_prv, term()}}.
do(State) ->
    try
        rebar_api:info("Looking for types to add...", []),
        CmdLineOpts = parse_opts(State),
        RebarConfigOpts = parse_rebar_config(State),
        Opts = maps:merge(RebarConfigOpts, CmdLineOpts),
        ok = rebar_api:debug("Opts: ~p", [Opts]),
        rebar3_mini_typer:run(Opts, State)
    catch
        error:{unrecognized_opt, Opt} ->
            {error, {?MODULE, {unrecognized_opt, Opt}}}
    end.

-spec format_error(any()) -> iolist().
format_error(not_implemented) ->
    io_lib:format("Not yet implemented.", []);
format_error({unrecognized_opt, Opt}) ->
    io_lib:format("Unrecognized option in rebar.config: ~p", [Opt]);
format_error({colliding_modes, NewMode, OldMode}) ->
    io_lib:format("Mode was previously set to '~p'; cannot set it to '~p' now",
                  [OldMode, NewMode]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

parse_opts(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    parse_cli_opts(CliOpts, #{mode => show}).

parse_cli_opts([], Acc) ->
    Acc;
parse_cli_opts([{recursive, Dirs} | T], Acc) ->
    parse_cli_opts(T, Acc#{files_r => split_string(Dirs)});
parse_cli_opts([{show, _Bool} | T], Acc) ->
    parse_cli_opts(T, Acc);
parse_cli_opts([{show_exported, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(show_exported, Bool, Acc));
parse_cli_opts([{annotate, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(annotate, Bool, Acc));
parse_cli_opts([{annotate_inc_files, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(annotate_inc_files, Bool, Acc));
parse_cli_opts([{show_success_typings, Bool} | T], Acc) ->
    parse_cli_opts(T, Acc#{show_succ => Bool});
parse_cli_opts([{no_spec, Bool} | T], Acc) ->
    parse_cli_opts(T, Acc#{no_spec => Bool});
parse_cli_opts([{edoc, Bool} | T], Acc) ->
    parse_cli_opts(T, Acc#{edoc => Bool});
parse_cli_opts([{plt, PltFile} | T], Acc) ->
    parse_cli_opts(T, Acc#{plt => PltFile});
parse_cli_opts([{typespec_files, Files} | T], Acc) ->
    parse_cli_opts(T, Acc#{trusted => split_string(Files)});
parse_cli_opts([Opt | _T], _Acc) ->
    %% consider changing this to the rebar3 way of ?PRV_ERROR/1
    %% TODO: catch the error
    error({unrecognized_opt, Opt}).

set_mode(Key, false, Acc = #{mode := Key}) ->
    maps:remove(Key, Acc);
set_mode(_Key, false, Acc = #{mode := _OtherMode}) ->
    Acc;
set_mode(_Key, false, Acc) ->
    Acc;
set_mode(Key, true, Acc = #{mode := show}) ->
    Acc#{mode => Key};
set_mode(Key, true, Acc = #{mode := Key}) ->
    Acc;
set_mode(Key, true, #{mode := OtherKey}) ->
    error({colliding_modes, Key, OtherKey});
set_mode(Key, true, Acc) ->
    Acc#{mode => Key}.

split_string(String) ->
    rebar_string:lexemes(String, [$,]).

%% @todo consider adding shorthand versions to some (or all) options,
%%       even if it doesn't exist on TypEr itself
opts() ->
    [{recursive,
      $r,
      undefined,
      string,
      "Search comma-separated directories recursively for .erl files below them."},
     {show,
      undefined,
      "show",
      {boolean, true},
      "Print type specifications for all functions on stdout."},
     {show_exported,
      undefined,
      "show-exported",
      {boolean, false},
      "Same as --show, but print specifications for exported functions only."
      "Specs are displayed sorted alphabetically on the function's name."},
     {show_exported, undefined, "show_exported", {boolean, false}, "Same as --show-exported."},
     {show_success_typings,
      undefined,
      "show-success-typings",
      {boolean, false},
      "Show the success typings inferred by Dialyzer / Typer. This is an undocumented option."},
     {show_success_typings,
      undefined,
      "show_success_typings",
      {boolean, false},
      "Same as --show-success-typings. This is an undocumented option."},
     {annotate,
      undefined,
      "annotate",
      {boolean, false},
      "Annotate the specified files with type specifications."},
     {annotate_inc_files,
      undefined,
      "annotate-inc-files",
      {boolean, false},
      "Same as --annotate but annotates all -include() files as well as all .erl files."
      " (Use this option with caution - it has not been tested much)."},
     {no_spec,
      undefined,
      "no_spec",
      {boolean, false},
      "Ignore the specs from the files. This is an undocumented option."},
     {edoc,
      undefined,
      "edoc",
      {boolean, false},
      "Print type information as Edoc @spec comments, not as type specs."},
     {plt,
      undefined,
      "plt",
      string,
      "Use the specified dialyzer PLT file rather than the default one."},
     {typespec_files,
      $T,
      undefined,
      string,
      "The specified file(s) already contain type specifications and these are to be trusted "
      "in order to print specs for the rest of the files. (Multiple files or dirs, separated "
      "by commas, can be specified.)"}].

parse_rebar_config(State) ->
    Config = rebar_state:get(State, typer, []),
    lists:foldl(fun parse_rebar_config/2, #{}, proplists:unfold(Config)).

parse_rebar_config({show_success_typings, Value}, Opts) ->
    Opts#{show_succ => Value};
parse_rebar_config({typespec_files, Value}, Opts) ->
    Opts#{trusted => Value};
parse_rebar_config({Key, Value}, Opts)
    when Key == mode;
         Key == edoc;
         Key == plt;
         Key == typespec_files;
         Key == no_spec;
         Key == recursive ->
    Opts#{Key => Value};
parse_rebar_config(Opt, _Opts) ->
    error({unrecognized_opt, Opt}).
