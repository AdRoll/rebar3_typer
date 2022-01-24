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

You can basically use the same command-line options as you can use with the original TypEr:

```man
Usage: rebar3 typer [-r <recursive>] [--show [<show>]]
                    [--show-exported [<show_exported>]]
                    [--show_exported [<show_exported>]]
                    [--show-success-typings [<show_success_typings>]]
                    [--show_success_typings [<show_success_typings>]]
                    [--annotate [<annotate>]]
                    [--annotate-inc-files [<annotate_inc_files>]]
                    [--no_spec [<no_spec>]] [--edoc [<edoc>]]
                    [--plt <plt>] [-T <typespec_files>] [-v]

  -r                      Search comma-separated directories recursively
                          for .erl files below them.
  --show                  Print type specifications for all functions on
                          stdout. [default: true]
  --show-exported         Same as --show, but print specifications for
                          exported functions only.Specs are displayed
                          sorted alphabetically on the function's name.
                          [default: false]
  --show_exported         Same as --show-exported. [default: false]
  --show-success-typings  Show the success typings inferred by Dialyzer /
                          Typer. This is an undocumented option. [default:
                          false]
  --show_success_typings  Same as --show-success-typings. This is an
                          undocumented option. [default: false]
  --annotate              Annotate the specified files with type
                          specifications. [default: false]
  --annotate-inc-files    Same as --annotate but annotates all -include()
                          files as well as all .erl files. (Use this
                          option with caution - it has not been tested
                          much). [default: false]
  --no_spec               Ignore the specs from the files. This is an
                          undocumented option. [default: false]
  --edoc                  Print type information as Edoc @spec comments,
                          not as type specs. [default: false]
  --plt                   Use the specified dialyzer PLT file rather than
                          the default one.
  -T                      The specified file(s) already contain type
                          specifications and these are to be trusted in
                          order to print specs for the rest of the files.
                          (Multiple files or dirs, separated by commas,
                          can be specified.)
```

You can also put those options directly in your `rebar.config` file…

```erlang
{typer,
    [{mode, show},                  %% Results mode: show | show_exported | annotate | annotate_inc_files
     {edoc, false},                 %% Print type information as Edoc @spec comments, not as type specs.
     {plt, "/path/to/plt"},         %% Use the specified dialyzer PLT file rather than the default one.
     {show_success_typings, false}, %% Show the success typings inferred by Dialyzer / Typer.
     {no_spec, false},              %% Ignore existing function specs.
     {typespec_files, ["f1", "f2"]} %% The specified file(s) already contain type specifications.
    ]}
```
