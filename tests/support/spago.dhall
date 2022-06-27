{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, backend = "purerl"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "erl-lists"
  , "erl-modules"
  , "erl-test-eunit"
  , "erl-tuples"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "functions"
  , "maybe"
  , "partial"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [] : List Text
}
