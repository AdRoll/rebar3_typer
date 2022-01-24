# rebar3_typer

A rebar3 plugin wrapper around [TypEr](https://www.erlang.org/doc/man/typer.html)

## Build and Test

```sh
$ rebar3 test
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_typer, "~> 0.0.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 typer
===> Fetching rebar3_typer
===> Compiling rebar3_typer
<Plugin Output>
```

## Configuration

<!-- @pablocostas text after #5 is merged -->

You can also put those options directly in your `rebar.config` file…

```erlang
{typer,
    [{show, true},                  %% Print type specifications for all functions on stdout.
     {show_exported, false},        %% Same as --show, but print specifications for exported functions only.
     {annotate, false},             %% Annotate the specified files with type specifications.
     {annotate_inc_files, false},   %% Same as --annotate but annotates all -include() files as well as all .erl files.
     {edoc, false},                 %% Print type information as Edoc @spec comments, not as type specs.
     {plt, "/path/to/plt"},         %% Use the specified dialyzer PLT file rather than the default one.
     {show_success_typings, false}, %% Show the success typings inferred by Dialyzer / Typer.
     {no_spec, false},              %% Ignore existing function specs.
     {typespec_files, ["f1", "f2"]} %% The specified file(s) already contain type specifications.
    ]}
```
