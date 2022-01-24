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
