%%% @doc Plugin provider for rebar3 rebar3_typer.
-module(rebar3_typer_prv).

-export([init/1, do/1, format_error/1]).

-ignore_xref([do/1,
              format_error/1,
              {providers, create, 1},
              {rebar_state, add_provider, 2},
              {rebar_state, command_parsed_args, 1}]).

-define(PROVIDER, typer).
-define(OPTS, %% TODO: consider adding shorthand version to some (or all) options, even if it doesn't exist on TypEr itself
        [{recursive,
          undefined,
          "recursive",
          {boolean, false},
          "Search directories recursively for .erl files below them."},
         {show,
          undefined,
          "show",
          {boolean, false},
          "Print type specifications for all functions on stdout. (This is the default behaviour; this option is not really needed.)"},
         {show_exported,
          undefined,
          "show-exported",
          {boolean, false},
          "Same as --show, but print specifications for exported functions only. Specs are displayed sorted alphabetically on the function's name."},
         {annotate,
          undefined,
          "annotate",
          {boolean, false},
          "Annotate the specified files with type specifications."},
         {annotate_inc_files,
          undefined,
          "annotate-inc-files",
          {boolean, false},
          "Same as --annotate but annotates all -include() files as well as all .erl files. (Use this option with caution - it has not been tested much)."},
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
          "The specified file(s) already contain type specifications and these are to be trusted in order to print specs for the rest of the files. (Multiple files or dirs, separated by spaces, can be specified.)"},
         {version,
          $v,
          "version",
          string,
          "Print the TypEr version and some more information and exit."}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER}, % The 'user friendly' name of the task
                          {module, ?MODULE}, % The module implementation of the task
                          {bare, true},      % The task can be run by the user, always true
                          {deps, []},        % The list of dependencies
                          {example, "rebar3 typer"}, % How to use the plugin
                          {opts, ?OPTS},     % list of options understood by the plugin
                          {short_desc, "Execute TypEr on your code"},
                          {desc, "Execute TypEr on your code"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Opts = parse_opts(State),
    ok = rebar_api:debug("Opts: ~p", [Opts]),
    case Opts of
        #{boolean := true} ->
            {ok, State};
        _ ->
            {error, io_lib:format("Not implemented yet. Opts: ~p", [Opts])}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec parse_opts(rebar_state:t()) -> maps:map().
parse_opts(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    #{boolean => proplists:get_value(boolean, Args),
      string => proplists:get_value(string, Args),
      atom => proplists:get_value(atom, Args),
      atom_none => proplists:get_value(atom_none, Args)}.
