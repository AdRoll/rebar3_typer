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
                          {deps, [app_discovery]},
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
        Merged = maps:merge(RebarConfigOpts, CmdLineOpts),
        Opts = ensure_defaults(Merged, State),
        ok = rebar3_mini_typer:run(Opts),
        {ok, State}
    catch
        error:{unrecognized_opt, Opt} ->
            {error, {?MODULE, {unrecognized_opt, Opt}}};
        error:{colliding_modes, NewMode, OldMode} ->
            {error, {?MODULE, {colliding_modes, NewMode, OldMode}}}
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
    parse_cli_opts(CliOpts, #{}).

parse_cli_opts([], Acc) ->
    Acc;
parse_cli_opts([{recursive, Dirs} | T], Acc) ->
    parse_cli_opts(T, Acc#{files_r => split_string(Dirs)});
parse_cli_opts([{show, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(show, Bool, Acc));
parse_cli_opts([{show_exported, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(show_exported, Bool, Acc));
parse_cli_opts([{annotate, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(annotate, Bool, Acc));
parse_cli_opts([{annotate_inc_files, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(annotate_inc_files, Bool, Acc));
parse_cli_opts([{annotate_in_place, Bool} | T], Acc) ->
    parse_cli_opts(T, set_mode(annotate_in_place, Bool, Acc));
parse_cli_opts([{show_success_typings, Bool} | T], Acc) ->
    parse_cli_opts(T, Acc#{show_succ => Bool});
parse_cli_opts([{no_spec, Bool} | T], Acc) ->
    parse_cli_opts(T, Acc#{no_spec => Bool});
parse_cli_opts([{edoc, Bool} | T], Acc) ->
    parse_cli_opts(T, Acc#{edoc => Bool});
parse_cli_opts([{plt, PltFile} | T], Acc) ->
    parse_cli_opts(T, Acc#{plt => PltFile});
parse_cli_opts([{typespec_files, Files} | T], Acc) ->
    parse_cli_opts(T, Acc#{trusted => split_string(Files)}).

set_mode(Key, false, Acc = #{mode := Key}) ->
    maps:remove(mode, Acc);
set_mode(_Key, false, Acc = #{mode := _OtherMode}) ->
    Acc;
set_mode(_Key, false, Acc) ->
    Acc;
set_mode(Key, true, Acc = #{mode := Key}) ->
    Acc;
set_mode(Key, true, #{mode := OtherKey}) ->
    error({colliding_modes, Key, OtherKey});
set_mode(Key, true, Acc) ->
    Acc#{mode => Key}.

split_string(String) ->
    rebar_string:lexemes(String, [$,]).

%% Setting default values _after_ the CLI and config file settings
%% have been merged, because if we set it in the CLI defaults,
%% the config file can't override them. We need to only set them
%% if they're not set in either place.
-spec ensure_defaults(map(), rebar_state:t()) -> rebar3_mini_typer:opts().
ensure_defaults(Opts, State) ->
    default_plt(default_src_dirs(default_io(default_mode_show(Opts)), State), State).

-spec default_io(rebar3_mini_typer:opts()) -> rebar3_mini_typer:opts().
default_io(Opts) ->
    Opts#{io =>
              #{debug => fun rebar_api:debug/2,
                info => fun rebar_api:info/2,
                warn => fun rebar_api:warn/2,
                abort => fun rebar_api:abort/2}}.

-spec default_mode_show(map()) -> rebar3_mini_typer:opts().
default_mode_show(#{mode := _Anything} = Opts) ->
    Opts;
default_mode_show(Opts) ->
    Opts#{mode => show}.

-spec default_plt(map(), rebar_state:t()) -> rebar3_mini_typer:opts().
default_plt(#{plt := _Anything} = Opts, _State) ->
    Opts;
default_plt(#{} = Opts, State) ->
    Opts#{plt => get_plt(State)}.

-spec get_plt(rebar_state:t()) -> file:filename_all().
get_plt(State) ->
    %% Dialyzer lets a directory and a prefix be specified in rebar.config
    %% So, check for those, and otherwise use the default:
    %% base_dir/rebar3_{otp_version}_plt
    DialyzerConfig = rebar_state:get(State, dialyzer, []),
    Dir = case proplists:get_value(plt_location, DialyzerConfig, undefined) of
              local ->
                  rebar_dir:base_dir(State);
              undefined ->
                  rebar_dir:base_dir(State);
              Location ->
                  Location
          end,
    Prefix =
        case proplists:get_value(plt_prefix, DialyzerConfig, undefined) of
            undefined ->
                "rebar3";
            Pre ->
                Pre
        end,
    Filename = Prefix ++ "_" ++ rebar_utils:otp_release() ++ "_plt",
    filename:join(Dir, Filename).

-spec default_src_dirs(rebar3_mini_typer:opts(), rebar_state:t()) ->
                          rebar3_mini_typer:opts().
default_src_dirs(#{files_r := _Anything} = Opts, _State) ->
    Opts;
default_src_dirs(#{} = Opts, State) ->
    case dirs_from_app_discovery(State) of
        [] ->
            Opts#{files_r => infer_src_dirs(State)};
        Dirs ->
            Opts#{files_r => Dirs}
    end.

infer_src_dirs(State) ->
    SrcDirs = rebar_state:get(State, src_dirs, []),
    Extra = rebar_state:get(State, extra_src_dirs, []),
    SubDirs = rebar_state:get(State, sub_dirs, []),
    FromState = SrcDirs ++ Extra ++ SubDirs,
    case FromState of
        [] -> % last ditch
            filelib:wildcard("src") ++ filelib:wildcard("{lib,apps}/*/src");
        Dirs ->
            Dirs
    end.

dirs_from_app_discovery(State) ->
    [dir_for_app(AppInfo) || AppInfo <- rebar_state:project_apps(State)].

-spec dir_for_app(rebar_app_info:t()) -> file:filename_all() | [].
dir_for_app(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    rebar_dir:make_relative_path(
        filename:join(Dir, "src"), rebar_dir:get_cwd()).

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
      {boolean, false},
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
     {annotate_in_place,
      undefined,
      "annotate-in-place",
      {boolean, false},
      "Annotate directly on the source code files, instead of dumping the annotated files in a"
      " different directory."},
     {annotate_in_place,
      undefined,
      "annotate_in_place",
      {boolean, false},
      "Same as --annotate-in-place."},
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
      "Use the specified dialyzer PLT file rather than the default one from"
      " the profile's base directory."},
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

parse_rebar_config({recursive, Value}, Opts) ->
    Opts#{files_r => Value};
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
