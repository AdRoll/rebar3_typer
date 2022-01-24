%%% @doc Plugin provider for rebar3 rebar3_typer.
-module(rebar3_typer_prv).

-export([init/1, do/1, format_error/1]).

-ignore_xref([do/1,
              format_error/1,
              {providers, create, 1},
              {rebar_state, add_provider, 2},
              {rebar_state, command_parsed_args, 1}]).

-define(PROVIDER, typer).
-define(OPTS,
        [{boolean, $b, "boolean", {boolean, false}, "Boolean example"},
         {string, $s, "rebar-config", {string, "some.txt"}, "String example"},
         {atom, $a, "ignore", atom, "Atom example"},
         {atom_none, $n, "atom-none", {atom, none}, "Atom example, with default"}]).

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
    try
        CmdLineOpts = parse_opts(State),
        RebarConfigOpts = parse_rebar_config(State),
        Opts = maps:merge(RebarConfigOpts, CmdLineOpts),
        ok = rebar_api:debug("Opts: ~p", [Opts]),
        {error, io_lib:format("Not implemented yet. Opts: ~p", [Opts])}
    catch
        error:{unrecognized_opt, Opt} ->
            {error, io_lib:format("Unrecognized option in rebar.config: ~p", [Opt])}
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

parse_rebar_config(State) ->
    Config = rebar_state:get(State, typer, []),
    lists:foldl(fun parse_rebar_config/2, #{}, proplists:unfold(Config)).

parse_rebar_config({Key, Value}, Opts)
    when Key == show;
         Key == show_exported;
         Key == annotate;
         Key == annotate_inc_files;
         Key == edoc;
         Key == plt;
         Key == typespec_files;
         Key == show_success_typings;
         Key == no_spec ->
    Opts#{Key => Value};
parse_rebar_config(Opt, _Opts) ->
    error({unrecognized_opt, Opt}).
