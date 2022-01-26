%%% @doc Test module for rebar3_typer_prv
-module(rebar3_typer_prv_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([no_options/1, recursive/1, good_modes/1, colliding_modes/1,
         show_success_typings/1, no_spec/1, edoc/1, plt/1, typespec_files/1, unrecognized_opt/1,
         format_error/1]).

all() ->
    [no_options,
     recursive,
     good_modes,
     colliding_modes,
     show_success_typings,
     no_spec,
     edoc,
     plt,
     typespec_files,
     unrecognized_opt,
     format_error].

init_per_testcase(_, Config) ->
    Self = self(),
    meck:new(rebar3_mini_typer),
    meck:expect(rebar3_mini_typer,
                run,
                fun(Opts) ->
                   Self ! #{opts => Opts},
                   ok
                end),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(rebar3_mini_typer),
    Config.

%% @doc Just try to run typer without options
no_options(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("Simply running typer without any parameter should use only default values"),
    [{mode, show}] = get_opts(State),

    {comment, ""}.

%% @doc --recursive / recursive
recursive(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("files_r is correctly picked up from rebar.config"),
    Files = ["src/", "test/"],
    State1 = rebar_state:set(State, typer, [{recursive, Files}]),
    {files_r, Files} = lists:keyfind(files_r, 1, get_opts(State1)),

    ct:comment("--recursive takes precedence"),
    State2 = rebar_state:command_parsed_args(State1, {[{recursive, "lib/,src/"}], []}),
    {files_r, ["lib/", "src/"]} = lists:keyfind(files_r, 1, get_opts(State2)),
    {comment, ""}.

%% @doc --show|show_exported|annotate|annotate_inc_files / mode
good_modes(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("mode is correctly picked up from rebar.config"),
    State1 = rebar_state:set(State, typer, [{mode, annotate}]),
    {mode, annotate} = lists:keyfind(mode, 1, get_opts(State1)),

    State2 = rebar_state:set(State1, typer, [{mode, show_exported}]),
    {mode, show_exported} = lists:keyfind(mode, 1, get_opts(State2)),

    ct:comment("--show takes precedence"),
    State3 = rebar_state:command_parsed_args(State2, {[{show, true}], []}),
    {mode, show} = lists:keyfind(mode, 1, get_opts(State3)),

    ct:comment("--show=false uses what's in rebar.config"),
    State4 = rebar_state:command_parsed_args(State2, {[{show, false}], []}),
    {mode, show_exported} = lists:keyfind(mode, 1, get_opts(State4)),

    ct:comment("--annotate works"),
    State5 = rebar_state:command_parsed_args(State2, {[{annotate, true}], []}),
    {mode, annotate} = lists:keyfind(mode, 1, get_opts(State5)),

    ct:comment("--show_exported works"),
    State6 = rebar_state:command_parsed_args(State2, {[{show_exported, true}], []}),
    {mode, show_exported} = lists:keyfind(mode, 1, get_opts(State6)),

    ct:comment("--annotate-inc-files works"),
    State7 = rebar_state:command_parsed_args(State2, {[{annotate_inc_files, true}], []}),
    {mode, annotate_inc_files} = lists:keyfind(mode, 1, get_opts(State7)),

    ct:comment("on and off works"),
    State8 =
        rebar_state:command_parsed_args(State, %% without changes in rebar.config
                                        {[{show_exported, true}, {show_exported, false}], []}),
    {mode, show} = lists:keyfind(mode, 1, get_opts(State8)),

    ct:comment("many false ones"),
    State9 =
        rebar_state:command_parsed_args(State2,
                                        {[{annotate, true},
                                          {annotate_inc_files, false},
                                          {show, false}],
                                         []}),
    {mode, annotate} = lists:keyfind(mode, 1, get_opts(State9)),

    ct:comment("super true"),
    StateA =
        rebar_state:command_parsed_args(State2,
                                        {[{annotate, true}, {annotate, true}, {annotate, true}],
                                         []}),
    {mode, annotate} = lists:keyfind(mode, 1, get_opts(StateA)),
    {comment, ""}.

%% @doc --show|show_exported|annotate|annotate_inc_files / mode
colliding_modes(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("2 modes can't be set simultaneously"),
    State1 =
        rebar_state:command_parsed_args(State, {[{show, true}, {show_exported, true}], []}),
    {colliding_modes, show_exported, show} = get_error(State1),

    ct:comment("3 modes can't be set simultaneously"),
    State2 =
        rebar_state:command_parsed_args(State1,
                                        {[{show_exported, true},
                                          {annotate, true},
                                          {annotate_inc_files, true}],
                                         []}),
    {colliding_modes, annotate, show_exported} = get_error(State2),
    {comment, ""}.

%% @doc --show_success_typings / show_success_typings
show_success_typings(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("show_succ is correctly picked up from rebar.config"),
    State1 = rebar_state:set(State, typer, [{show_success_typings, true}]),
    {show_succ, true} = lists:keyfind(show_succ, 1, get_opts(State1)),

    ct:comment("--show_success_typings takes precedence"),
    State2 = rebar_state:command_parsed_args(State1, {[{show_success_typings, false}], []}),
    {show_succ, false} = lists:keyfind(show_succ, 1, get_opts(State2)),
    {comment, ""}.

%% @doc --no_spec / no_spec
no_spec(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("no_spec is correctly picked up from rebar.config"),
    State1 = rebar_state:set(State, typer, [{no_spec, true}]),
    {no_spec, true} = lists:keyfind(no_spec, 1, get_opts(State1)),

    ct:comment("--no_spec takes precedence"),
    State2 = rebar_state:command_parsed_args(State1, {[{no_spec, false}], []}),
    {no_spec, false} = lists:keyfind(no_spec, 1, get_opts(State2)),
    {comment, ""}.

%% @doc --edoc / edoc
edoc(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("edoc is correctly picked up from rebar.config"),
    State1 = rebar_state:set(State, typer, [{edoc, true}]),
    {edoc, true} = lists:keyfind(edoc, 1, get_opts(State1)),

    ct:comment("--edoc takes precedence"),
    State2 = rebar_state:command_parsed_args(State1, {[{edoc, false}], []}),
    {edoc, false} = lists:keyfind(edoc, 1, get_opts(State2)),
    {comment, ""}.

%% @doc --plt / plt
plt(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("plt is correctly picked up from rebar.config"),
    State1 = rebar_state:set(State, typer, [{plt, "1.plt"}]),
    {plt, "1.plt"} = lists:keyfind(plt, 1, get_opts(State1)),

    ct:comment("--plt takes precedence"),
    State2 = rebar_state:command_parsed_args(State1, {[{plt, "2.plt"}], []}),
    {plt, "2.plt"} = lists:keyfind(plt, 1, get_opts(State2)),
    {comment, ""}.

%% @doc --typespec_files / typespec_files
typespec_files(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("trusted is correctly picked up from rebar.config"),
    Files = ["f1.erl", "f2.erl"],
    State1 = rebar_state:set(State, typer, [{typespec_files, Files}]),
    {trusted, Files} = lists:keyfind(trusted, 1, get_opts(State1)),

    ct:comment("--typespec_files takes precedence"),
    State2 =
        rebar_state:command_parsed_args(State1, {[{typespec_files, "f3.erl,f4.erl"}], []}),
    {trusted, ["f3.erl", "f4.erl"]} = lists:keyfind(trusted, 1, get_opts(State2)),
    {comment, ""}.

%% @doc unrecognized options
unrecognized_opt(_Config) ->
    {ok, State} =
        rebar3_typer:init(
            rebar_state:new()),

    ct:comment("bad_opt in rebar.config"),
    State1 = rebar_state:set(State, typer, [{bad_opt, true}]),
    {unrecognized_opt, {bad_opt, true}} = get_error(State1),

    ct:comment("bad_opt in command line"),
    State2 = rebar_state:command_parsed_args(State1, {[{other_bad_opt, false}], []}),
    {unrecognized_opt, {other_bad_opt, false}} = get_error(State2),
    {comment, ""}.

%% @doc Error formatting
format_error(_Config) ->
    <<"Not yet implemented.">> =
        iolist_to_binary(rebar3_typer_prv:format_error(not_implemented)),
    <<"Unrecognized option in rebar.config: x">> =
        iolist_to_binary(rebar3_typer_prv:format_error({unrecognized_opt, x})),
    <<"Mode was previously set to 'm1'; cannot set it to 'm2' now">> =
        iolist_to_binary(rebar3_typer_prv:format_error({colliding_modes, m2, m1})),
    <<"other">> = iolist_to_binary(rebar3_typer_prv:format_error(other)),
    {comment, ""}.

get_opts(State) ->
    {ok, _} = rebar3_typer_prv:do(State),
    DefaultIo =
        #{abort => fun rebar_api:abort/2,
          debug => fun rebar_api:debug/2,
          info => fun rebar_api:info/2,
          warn => fun rebar_api:warn/2},
    receive
        #{opts := #{io := DefaultIo} = Opts} ->
            lists:sort(
                maps:to_list(
                    maps:remove(io, Opts)))
    after 500 ->
        {error, timeout}
    end.

get_error(State) ->
    try rebar3_typer_prv:do(State) of
        {error, {rebar3_typer_prv, Error}} ->
            Error;
        Unexpected ->
            ct:fail("Unexpected: ~p", [Unexpected])
    catch
        error:Error ->
            Error
    end.