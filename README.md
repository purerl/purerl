# purerl - the PureScript Erlang backend

# Overview

PureScript is a small strongly typed programming language with expressive types, written in and inspired by Haskell; purerl is a PureScript backend targetting Erlang source. 

# Resources

- [PureScript discord](https://discord.gg/jNh7FxhcAj) channel `#purerl` - general discussion, support, news
- [purerl API docs](https://purerl.github.io/package-sets/index.html) - API docs generated from the latest package set
  - ~~[purerl Pursuit](https://pursuit.purerl.fun/) - a version of the PureScript pursuit documentation repository for purerl package sets~~ (no longer being kept up to date)
- The [purerl](https://github.com/purerl) organisation hosts ports of some core libraries.
- The [purerl-cookbook](https://purerl-cookbook.readthedocs.io) contains some code examples and explanations.

# Versions

Currently the `purerl` executable should correspond to `purs` compiler versions as follows:

| `purerl` version | `purs` version |
| --- |  --- |
| 0.0.17 | 0.15.3 |
| 0.0.14-0.0.16 | 0.14.5 |
| 0.0.12-0.0.13 | 0.14.4 |
| 0.0.11 | 0.14.3 |
| 0.0.10 | 0.14.2 |
| 0.0.9 | 0.14.1 |
| 0.0.8 | 0.14.0 |
| 0.0.7 | 0.13.8 |
| 0.0.6 | 0.13.6 |

# Installation

Binaries are available for download on the [github releases](https://github.com/purerl/purerl/releases).

A nix overlay is also available at [nixpkgs-purerl](https://github.com/purerl/nixpkgs-purerl/), with related packages available at [nixpkgs-purerl-support](https://github.com/id3as/nixpkgs-purerl-support) (or [easy-purescript-nix](https://github.com/justinwoo/easy-purescript-nix)).

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
      https://github.com/purerl/package-sets/releases/download/erl-0.14.4-20211012-1/packages.dhall
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

## IDE support

The standard PureScript IDE tooling based on `purs ide` and editor plugins and/or [PureScript Language Server](https://github.com/nwolverson/purescript-language-server) should work with purerl projects, but should be configured to generate `corefn` rather than `js`, eg with vscode:

```json
"purescript.codegenTargets": [ "corefn" ]
```

# purerl erlang representation and FFI usage

## Types

The compiler knows of primitive types and the corresponding Erlang types, and some others for codegen optimisations, some other library types are included here for reference.

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
| `Effect.Uncurried.EffectFnX` | Function (arity `X`) | Actual higher arity functions (with effectful behaviour) | 
| `Erl.Data.List`  | `list()`| Native lists via  `purescript-erl-lists` |
| `Erl.Data.Tuple` | `tuple()` | Native tuples via `purescript-erl-tuples` |
| `Erl.Data.Map` | `#{tkey() => tvalue()}` | Map with homogenous key/value types |
| `Erl.Data.Binary` | `binary()` | Abitrary binaries (see also `IOList`) |

### Top level representation

Top level declarations are uniformly output as nullary functions, where arguments are curried as in the PureScript types. Identifiers are preserved, with quoting if required. Thus a normal invocation of the output will look like `(main@ps:main())()`. An uncurried version is also provided (and used in generated code), see below.

There is special handling of values of the following types at the top level:

* `Data.Function.Uncurried.FnX` 
* `Data.Function.Uncurried.EffectFnX`

These will generate code as for an `X` arity function, i.e. an arity `X` overload will be generated directly, being suitable for usage in callback module implementations, allowing for code replacement, etc.

## Modules

Module names `Foo.Bar` are transformed to a lower-snake cased form `foo_bar` (any non-initial uppercase chars will be preserved as such), with a suffix `@ps` to avoid clashing with built-in erlang modules.

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

## Calling purerl code from erlang

Following the module name mangling and representation above, calling a purerl function defined as follows

```purescript

module Foo.Bar where

import Prelude

appendNumber :: String -> Int -> String
appendNumber str n = str <> show n

```

goes something like this:

```erlang
-module(my_module).

f() -> ((foo_bar@ps:appendNumber())(<<"hello">>))(42).

```

Note that `foo_bar@ps:appendNumber()` represents the function value, and we call that like a chain of arity-1 erlang functions as according to the number of curried arguments. Parentheses soon stack up due to erlang precendence. Fortunately, an uncurried overload is generated:

```erlang
-module(my_module).

f() -> foo_bar@ps:appendNumber(<<"hello">>, 42).

```

These overloads should be generated at least for exported functions. 

Note that functions with type class constraints should not be called from outside of PureScript, similar to the guidelines for FFI functions.

## Generated types

Several `.hrl` files are generated in `output`, with the intention of improving dialyzer analysis. For a module `Foo.Bar`

* `foo_bar.hrl` - All types used in the module, this is imported by `foo_bar@foreign.hrl` and `foo_bar@ps`
* `foo_bar@foreign.hrl` - Types of the foreign imports of the module - note that currently these are only able to be generated when the imports are exported by the PureScript module

You may wish to import `foo_bar@foreign.hrl` in your FFI file in order to check the types of your imports, though bear in mind the `.erl` file will be copied from the source to `output/` - either some creative library paths or copying of the generated `hrl` will be required.
