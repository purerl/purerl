[![PureScript](assets/purescript-logo.png)](http://purescript.org) [![Erlang](assets/erlang-logo.svg | height=65 )](https://www.erlang.org/)

# purerl - the PureScript Erlang backend

# WIP

This backend (as opposed to compiler fork) is currently in an experimental state.

# Overview

A small strongly typed programming language with expressive types, written in and inspired by Haskell.

The original PureScript project compiles to JavaScript, but this fork is a backend targetting Erlang source. The [purerl](https://github.com/purerl) organisation hosts ports of some core libraries.

# Usage

The `purerl` backend requires the [mainline PureScript compiler (`purs`)](https://github.com/purescript/purescript) to operate. It generates `.erl` source files from the CoreFn `purs` compiler output (and `externs.json` type information), which can then be compiled by `erlc`.

The recommended usage is with `spago`, which now supports alternative backends. In `spago.dhall` set
```dhall
{ name = "my-project",
, backend = "purerl"
, ...
}
```
and in `packages.dhall` include a `purerl`-specific package set, e.g.
```
let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.5-20191204/src/packages.dhall sha256:308bc2dd2c5700e6e22fae85b10472fd76ddaaf8034711ff0eaede145573479a
```

Then 

* `spago build` will build the project through to `.erl`
* `spago run` will run the project from a given entry point

## Usage without `spago`

First compile to the CoreFn representation, e.g.
* `purs compile 'my_deps/**/*.purs' 'src/**/*.purs' --codegen corefn`
* `pulp build -- --codegen corefn`
* `psc-package build -- --codegen corefn`

Then run `purerl` to generate `.erl` files
* `purerl`

(No parameters are required, `purerl` will build whatever is in `output/`).

# purerl erlang representation and FFI usage

Module names `Foo.Bar` are transformed to a lower-snake cased form `foo_bar` (any non-initial uppercase chars will be preserved as such), with a suffix `@ps` to avoid clashing with built-in erlang modules.

Top level declarations are uniformly output as nullary functions. Identifiers are preserved, with quoting if required. Thus a normal invocation of the output will look like `(main@ps:main())()`.

## Types

| PureScript type | Erlang type | Notes |
| --- | --- | --- |
| `Int` | `integer()` | Arbitrary precision - no longer a `Bounded` |
| `Number` | `float()` |
| `Boolean` | `boolean()` |
| `String` | `binary()` | (`utf8` encoded) |
| `Array` | `array()` | Not to be confused with erlang `[]` list syntax. |
| Records | `#{atom() => any()}` | Map keyed by atoms |
| Tagged union | Tuple with tag element | e.g. `Some 42` is `{some, 42}` |
| Newtype | as underlying type |
| Functions | Function (arity 1 - but see FFI) |
| `Data.Function.Uncurried.FnX` | Function (arity `X`) | Actual higher arity functions - for 'uncurried' functions from tuples see `Erl.Data.Tuple`  | 
| `Erl.Data.List`  | `list()`| Native lists via  `purescript-erl-lists` |
| `Erl.Data.Tuple` | `tuple()` | Native tuples via `purescript-erl-tuples` |
| `Erl.Data.Map` | `tuple()` | Map with homogenous key/value types |

## FFI
In place of `.js` FFI files, the Erlang backend has `.erl` FFI files. As per the regular compiler since 0.9, these must be placed along the corresponding `.purs` file with the same name.


Module name: `Foo.MyModule`
PureScript file `Foo/MyModule.purs`
Erlang file: `Foo/MyModule.erl`
Erlang module: `foo_myModule@foreign`

Note that the FFI code for a module must not only be in a file named correctly, but the module must be named the same as the output module with `@foreign` appended (so *not* following the Erlang module naming requirement until this gets copied to output).

FFI files *MUST* export explicitly the exact set of identifiers which will be imported by the corresponding PureScript file. The compiler will check these exports and use them to inform codegen.

*Auto-currying*: functions can be defined with any arity. According to the arity of the export (parsed from the export list) the compiler will automatically apply to the right number of arguments. By extension, values are exported as a function of arity 0 returning that value.

An example:

```purescript
module Foo.Bar where

foreign import f :: Int -> Int -> Int -> Int
```

```erlang
-module(foo_bar@foreign).
-export([f/3]).

f(X, Y, Z) -> X + Y * Z.
```

This could also have been defined as
```erlang
-module(foo_bar@foreign).
-export([f/1]).

f(X) ->
  fun (Y) ->
    fun (Z) ->
      X + Y * Z
    end
  end.
```
