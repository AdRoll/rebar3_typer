# rebar3_typer

A rebar plugin description

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_typer, "~> 0.0.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 rebar3_typer
===> Fetching rebar3_typer
===> Compiling rebar3_typer
<Plugin Output>
```
