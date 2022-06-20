{ arrays =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "foldable-traversable"
    , "maybe"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purerl/purescript-arrays.git"
  , version = "v6.0.0-erl1"
  }
, `assert` =
  { dependencies = [ "console", "effect", "prelude" ]
  , repo = "https://github.com/purerl/purescript-assert.git"
  , version = "v5.0.0-erl1"
  }
, bifunctors =
  { dependencies = [ "const", "either", "newtype", "prelude", "tuples" ]
  , repo = "https://github.com/purescript/purescript-bifunctors.git"
  , version = "v5.0.0"
  }
, catenable-lists =
  { dependencies =
    [ "control"
    , "foldable-traversable"
    , "lists"
    , "maybe"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-catenable-lists.git"
  , version = "v6.0.1"
  }
, console =
  { dependencies = [ "effect", "prelude" ]
  , repo = "https://github.com/purerl/purescript-console.git"
  , version = "v5.0.0-erl1"
  }
, const =
  { dependencies = [ "invariant", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-const.git"
  , version = "v5.0.0"
  }
, contravariant =
  { dependencies = [ "const", "either", "newtype", "prelude", "tuples" ]
  , repo = "https://github.com/purescript/purescript-contravariant.git"
  , version = "v5.0.0"
  }
, control =
  { dependencies = [ "newtype", "prelude" ]
  , repo = "https://github.com/purerl/purescript-control.git"
  , version = "v5.0.0-erl1"
  }
, convertable-options =
  { dependencies = [ "effect", "maybe", "record" ]
  , repo = "https://github.com/natefaubion/purescript-convertable-options"
  , version = "v1.0.0"
  }
, datetime =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "functions"
    , "gen"
    , "integers"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "ordered-collections"
    , "partial"
    , "prelude"
    , "tuples"
    ]
  , repo = "https://github.com/purerl/purescript-datetime.git"
  , version = "v5.0.2-erl1"
  }
, datetime-parsing =
  { dependencies =
    [ "arrays"
    , "datetime"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "integers"
    , "lists"
    , "maybe"
    , "numbers"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "strings"
    ]
  , repo = "https://github.com/flounders/purescript-datetime-parsing"
  , version = "10c0a9aecc60a2a5e8cff35bebe45be4dacaa7f8"
  }
, debug =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purerl/purescript-debug.git"
  , version = "v5.0.0-erl1"
  }
, distributive =
  { dependencies =
    [ "identity", "newtype", "prelude", "tuples", "type-equality" ]
  , repo = "https://github.com/purescript/purescript-distributive.git"
  , version = "v5.0.0"
  }
, effect =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purerl/purescript-effect.git"
  , version = "v3.0.0-erl1"
  }
, either =
  { dependencies = [ "control", "invariant", "maybe", "prelude" ]
  , repo = "https://github.com/purescript/purescript-either.git"
  , version = "v5.0.0"
  }
, enums =
  { dependencies =
    [ "control"
    , "either"
    , "gen"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purerl/purescript-enums.git"
  , version = "v5.0.0-erl1"
  }
, erl-atom =
  { dependencies = [ "prelude", "unsafe-coerce" ]
  , repo = "https://github.com/purerl/purescript-erl-atom.git"
  , version = "v1.2.0"
  }
, erl-binary =
  { dependencies = [ "prelude", "maybe", "erl-lists" ]
  , repo = "https://github.com/purerl/purescript-erl-binary.git"
  , version = "v0.6.0"
  }
, erl-cowboy =
  { dependencies =
    [ "effect"
    , "either"
    , "erl-atom"
    , "erl-binary"
    , "erl-kernel"
    , "erl-lists"
    , "erl-maps"
    , "erl-modules"
    , "erl-ranch"
    , "erl-ssl"
    , "erl-tuples"
    , "foreign"
    , "functions"
    , "maybe"
    , "prelude"
    , "record"
    , "transformers"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purerl/purescript-erl-cowboy"
  , version = "v0.11.0"
  }
, erl-file =
  { dependencies = [ "erl-atom", "erl-binary", "prelude" ]
  , repo = "https://github.com/purerl/purescript-erl-file.git"
  , version = "v0.0.3"
  }
, erl-gun =
  { dependencies =
    [ "convertable-options"
    , "datetime"
    , "effect"
    , "either"
    , "erl-atom"
    , "erl-binary"
    , "erl-kernel"
    , "erl-lists"
    , "erl-maps"
    , "erl-process"
    , "erl-ssl"
    , "erl-tuples"
    , "erl-untagged-union"
    , "foreign"
    , "functions"
    , "maybe"
    , "prelude"
    , "record"
    , "simple-json"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/id3as/purescript-erl-gun.git"
  , version = "v0.0.2"
  }
, erl-jsone =
  { dependencies =
    [ "arrays", "integers", "assert", "either", "erl-lists", "erl-tuples" ]
  , repo = "https://github.com/purerl/purescript-erl-jsone"
  , version = "v0.4.0"
  }
, erl-kernel =
  { dependencies =
    [ "convertable-options"
    , "datetime"
    , "effect"
    , "either"
    , "erl-atom"
    , "erl-binary"
    , "erl-lists"
    , "erl-maps"
    , "erl-process"
    , "erl-tuples"
    , "erl-untagged-union"
    , "foldable-traversable"
    , "foreign"
    , "functions"
    , "integers"
    , "maybe"
    , "newtype"
    , "partial"
    , "prelude"
    , "record"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/id3as/purescript-erl-kernel.git"
  , version = "v0.0.3"
  }
, erl-lager =
  { dependencies = [ "erl-lists" ]
  , repo = "https://github.com/purerl/purescript-erl-lager.git"
  , version = "v0.0.1"
  }
, erl-lists =
  { dependencies =
    [ "prelude", "foldable-traversable", "unfoldable", "filterable", "tuples" ]
  , repo = "https://github.com/purerl/purescript-erl-lists.git"
  , version = "v4.0.1"
  }
, erl-logger =
  { dependencies = [ "prelude", "erl-atom", "erl-lists", "record" ]
  , repo = "https://github.com/id3as/purescript-erl-logger.git"
  , version = "v0.0.3"
  }
, erl-maps =
  { dependencies =
    [ "erl-lists", "functions", "prelude", "tuples", "unfoldable" ]
  , repo = "https://github.com/purerl/purescript-erl-maps.git"
  , version = "v0.5.0"
  }
, erl-modules =
  { dependencies = [ "erl-atom", "prelude", "strings" ]
  , repo = "https://github.com/purerl/purescript-erl-modules.git"
  , version = "v0.1.6"
  }
, erl-nativerefs =
  { dependencies = [ "prelude", "effect", "erl-tuples" ]
  , repo = "https://github.com/id3as/purescript-erl-nativerefs.git"
  , version = "v0.1.0"
  }
, erl-opentelemetry =
  { dependencies =
    [ "effect"
    , "erl-atom"
    , "erl-lists"
    , "erl-maps"
    , "erl-tuples"
    , "erl-untagged-union"
    , "maybe"
    , "prelude"
    , "tuples"
    , "unsafe-reference"
    ]
  , repo = "https://github.com/id3as/purescript-erl-opentelemetry.git"
  , version = "v0.0.1"
  }
, erl-otp-types =
  { dependencies =
    [ "erl-atom"
    , "erl-binary"
    , "erl-kernel"
    , "foreign"
    , "prelude"
    , "unsafe-reference"
    ]
  , repo = "https://github.com/id3as/purescript-erl-otp-types.git"
  , version = "v0.0.2"
  }
, erl-pinto =
  { dependencies =
    [ "erl-process"
    , "erl-lists"
    , "erl-atom"
    , "erl-kernel"
    , "datetime"
    , "erl-tuples"
    , "erl-modules"
    , "foreign"
    ]
  , repo = "https://github.com/id3as/purescript-erl-pinto.git"
  , version = "v0.2.0"
  }
, erl-process =
  { dependencies =
    [ "datetime", "effect", "either", "foreign", "integers", "prelude" ]
  , repo = "https://github.com/purerl/purescript-erl-process.git"
  , version = "v3.3.0"
  }
, erl-queue =
  { dependencies =
    [ "control"
    , "either"
    , "erl-lists"
    , "filterable"
    , "foldable-traversable"
    , "lists"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/id3as/purescript-erl-queue.git"
  , version = "v0.0.2"
  }
, erl-ranch =
  { dependencies =
    [ "convertable-options"
    , "effect"
    , "either"
    , "erl-atom"
    , "erl-kernel"
    , "erl-lists"
    , "erl-maps"
    , "erl-otp-types"
    , "erl-process"
    , "erl-ssl"
    , "erl-tuples"
    , "exceptions"
    , "foreign"
    , "maybe"
    , "prelude"
    , "record"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/id3as/purescript-erl-ranch.git"
  , version = "v0.0.2"
  }
, erl-simplebus =
  { dependencies = [ "effect", "erl-process", "maybe", "newtype", "prelude" ]
  , repo = "https://github.com/id3as/purescript-erl-simplebus.git"
  , version = "v0.0.3"
  }
, erl-ssl =
  { dependencies =
    [ "convertable-options"
    , "datetime"
    , "effect"
    , "either"
    , "maybe"
    , "erl-atom"
    , "erl-binary"
    , "erl-lists"
    , "erl-kernel"
    , "erl-tuples"
    , "erl-logger"
    , "erl-otp-types"
    , "foreign"
    , "maybe"
    , "partial"
    , "prelude"
    , "record"
    , "unsafe-reference"
    ]
  , repo = "https://github.com/id3as/purescript-erl-ssl.git"
  , version = "v0.0.2"
  }
, erl-stetson =
  { dependencies =
    [ "erl-atom"
    , "erl-binary"
    , "erl-lists"
    , "erl-maps"
    , "erl-tuples"
    , "erl-modules"
    , "erl-cowboy"
    , "foreign"
    , "maybe"
    , "prelude"
    , "transformers"
    , "routing-duplex"
    ]
  , repo = "https://github.com/id3as/purescript-erl-stetson.git"
  , version = "v0.13.0"
  }
, erl-test-eunit =
  { dependencies =
    [ "assert"
    , "console"
    , "debug"
    , "erl-lists"
    , "erl-tuples"
    , "erl-atom"
    , "foreign"
    , "free"
    , "prelude"
    , "psci-support"
    ]
  , repo = "https://github.com/id3as/purescript-erl-test-eunit.git"
  , version = "v0.0.4"
  }
, erl-test-eunit-discovery =
  { dependencies =
    [ "effect"
    , "erl-lists"
    , "erl-modules"
    , "erl-test-eunit"
    , "filterable"
    , "foldable-traversable"
    , "maybe"
    , "free"
    , "prelude"
    ]
  , repo = "https://github.com/id3as/purescript-erl-test-eunit-discovery.git"
  , version = "d0b6d9f5bcab13f79c3941c64e52ee86f7cd4e2b"
  }
, erl-tuples =
  { dependencies = [ "unfoldable", "tuples" ]
  , repo = "https://github.com/purerl/purescript-erl-tuples.git"
  , version = "v3.3.1"
  }
, erl-untagged-union =
  { dependencies =
    [ "erl-atom"
    , "erl-binary"
    , "erl-lists"
    , "erl-tuples"
    , "foreign"
    , "typelevel-prelude"
    , "maybe"
    , "partial"
    , "prelude"
    , "unsafe-coerce"
    , "erl-process"
    ]
  , repo = "https://github.com/id3as/purescript-erl-untagged-union.git"
  , version = "v0.0.2"
  }
, exceptions =
  { dependencies = [ "maybe", "either", "effect" ]
  , repo = "https://github.com/purerl/purescript-exceptions.git"
  , version = "v5.0.0-erl1"
  }
, exists =
  { dependencies = [ "unsafe-coerce" ]
  , repo = "https://github.com/purescript/purescript-exists.git"
  , version = "v5.1.0"
  }
, expect-inferred =
  { dependencies = [ "prelude", "typelevel-prelude" ]
  , repo = "https://github.com/justinwoo/purescript-expect-inferred"
  , version = "v2.0.0"
  }
, filterable =
  { dependencies =
    [ "arrays"
    , "either"
    , "foldable-traversable"
    , "identity"
    , "lists"
    , "ordered-collections"
    ]
  , repo = "https://github.com/purescript/purescript-filterable.git"
  , version = "v3.0.1"
  }
, foldable-traversable =
  { dependencies =
    [ "bifunctors"
    , "const"
    , "control"
    , "either"
    , "functors"
    , "identity"
    , "maybe"
    , "newtype"
    , "orders"
    , "prelude"
    , "tuples"
    ]
  , repo = "https://github.com/purerl/purescript-foldable-traversable.git"
  , version = "v5.0.1-erl1"
  }
, foreign =
  { dependencies =
    [ "either"
    , "functions"
    , "identity"
    , "integers"
    , "lists"
    , "maybe"
    , "prelude"
    , "strings"
    , "transformers"
    ]
  , repo = "https://github.com/purerl/purescript-foreign.git"
  , version = "v6.0.1-erl1"
  }
, formatters =
  { dependencies =
    [ "arrays"
    , "bifunctors"
    , "control"
    , "datetime"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "integers"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "numbers"
    , "ordered-collections"
    , "parsing"
    , "partial"
    , "prelude"
    , "psci-support"
    , "strings"
    , "transformers"
    , "tuples"
    ]
  , repo = "https://github.com/id3as/purescript-formatters"
  , version = "v5.0.1-erl1"
  }
, free =
  { dependencies =
    [ "catenable-lists"
    , "control"
    , "distributive"
    , "either"
    , "exists"
    , "foldable-traversable"
    , "invariant"
    , "lazy"
    , "maybe"
    , "prelude"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript/purescript-free.git"
  , version = "v6.0.1"
  }
, functions =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purerl/purescript-functions.git"
  , version = "v5.0.0-erl1"
  }
, functors =
  { dependencies =
    [ "bifunctors"
    , "const"
    , "contravariant"
    , "control"
    , "distributive"
    , "either"
    , "invariant"
    , "maybe"
    , "newtype"
    , "prelude"
    , "profunctor"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript/purescript-functors.git"
  , version = "v4.1.1"
  }
, gen =
  { dependencies =
    [ "either"
    , "foldable-traversable"
    , "identity"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-gen.git"
  , version = "v3.0.0"
  }
, graphs =
  { dependencies = [ "catenable-lists", "ordered-collections" ]
  , repo = "https://github.com/purescript/purescript-graphs.git"
  , version = "v5.0.0"
  }
, heterogeneous =
  { dependencies =
    [ "prelude", "record", "tuples", "functors", "variant", "either" ]
  , repo = "https://github.com/natefaubion/purescript-heterogeneous.git"
  , version = "v0.5.1"
  }
, identity =
  { dependencies = [ "control", "invariant", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-identity.git"
  , version = "v5.0.0"
  }
, integers =
  { dependencies = [ "math", "maybe", "numbers", "prelude" ]
  , repo = "https://github.com/purerl/purescript-integers.git"
  , version = "v5.0.0-erl1"
  }
, invariant =
  { dependencies = [ "control", "prelude" ]
  , repo = "https://github.com/purescript/purescript-invariant.git"
  , version = "v5.0.0"
  }
, js-uri =
  { dependencies = [ "functions", "maybe" ]
  , repo = "https://github.com/purerl/purescript-js-uri.git"
  , version = "v2.0.0-erl1"
  }
, lazy =
  { dependencies = [ "control", "foldable-traversable", "invariant", "prelude" ]
  , repo = "https://github.com/purerl/purescript-lazy.git"
  , version = "v5.0.0-erl1"
  }
, lcg =
  { dependencies =
    [ "effect", "integers", "math", "maybe", "partial", "prelude", "random" ]
  , repo = "https://github.com/purescript/purescript-lcg.git"
  , version = "v3.0.0"
  }
, lists =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "foldable-traversable"
    , "lazy"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-lists.git"
  , version = "v6.0.1"
  }
, math =
  { dependencies = [] : List Text
  , repo = "https://github.com/purerl/purescript-math.git"
  , version = "v3.0.0-erl1"
  }
, maybe =
  { dependencies = [ "control", "invariant", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-maybe.git"
  , version = "v5.0.0"
  }
, media-types =
  { dependencies = [ "prelude", "newtype" ]
  , repo = "https://github.com/purescript-contrib/purescript-media-types.git"
  , version = "v5.0.0"
  }
, metadata =
  { dependencies = [] : List Text
  , repo = "https://github.com/spacchetti/purescript-metadata.git"
  , version = "v0.15.0"
  }
, newtype =
  { dependencies = [ "prelude", "safe-coerce" ]
  , repo = "https://github.com/purescript/purescript-newtype.git"
  , version = "v4.0.0"
  }
, nonempty =
  { dependencies =
    [ "control"
    , "foldable-traversable"
    , "maybe"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-nonempty.git"
  , version = "v6.0.0"
  }
, nullable =
  { dependencies = [ "maybe", "functions" ]
  , repo = "https://github.com/purerl/purescript-nullable.git"
  , version = "v5.0.0-erl1"
  }
, numbers =
  { dependencies = [ "functions", "math", "maybe" ]
  , repo = "https://github.com/purerl/purescript-numbers.git"
  , version = "v8.0.0-erl1"
  }
, ordered-collections =
  { dependencies =
    [ "arrays"
    , "foldable-traversable"
    , "gen"
    , "lists"
    , "maybe"
    , "partial"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purerl/purescript-ordered-collections.git"
  , version = "v2.0.2-erl1"
  }
, orders =
  { dependencies = [ "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-orders.git"
  , version = "v5.0.0"
  }
, parallel =
  { dependencies =
    [ "control"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "functors"
    , "maybe"
    , "newtype"
    , "prelude"
    , "profunctor"
    , "refs"
    , "transformers"
    ]
  , repo = "https://github.com/purescript/purescript-parallel.git"
  , version = "v5.0.0"
  }
, parsing =
  { dependencies =
    [ "arrays"
    , "assert"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "identity"
    , "integers"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "prelude"
    , "psci-support"
    , "strings"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "unicode"
    ]
  , repo = "https://github.com/id3as/purescript-parsing"
  , version = "v6.0.2-erl1"
  }
, partial =
  { dependencies = [] : List Text
  , repo = "https://github.com/purerl/purescript-partial.git"
  , version = "v3.0.0-erl2"
  }
, pathy =
  { dependencies =
    [ "arrays"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "gen"
    , "identity"
    , "lists"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "psci-support"
    , "strings"
    , "tailrec"
    , "tuples"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/id3as/purescript-pathy"
  , version = "v8.1.0-erl1"
  }
, prelude =
  { dependencies = [] : List Text
  , repo = "https://github.com/purerl/purescript-prelude.git"
  , version = "v5.0.1-erl1"
  }
, profunctor =
  { dependencies =
    [ "control"
    , "distributive"
    , "either"
    , "exists"
    , "invariant"
    , "newtype"
    , "prelude"
    , "tuples"
    ]
  , repo = "https://github.com/purescript/purescript-profunctor.git"
  , version = "v5.0.0"
  }
, profunctor-lenses =
  { dependencies =
    [ "arrays"
    , "bifunctors"
    , "const"
    , "control"
    , "distributive"
    , "either"
    , "foldable-traversable"
    , "functors"
    , "identity"
    , "lists"
    , "maybe"
    , "newtype"
    , "ordered-collections"
    , "partial"
    , "prelude"
    , "profunctor"
    , "record"
    , "transformers"
    , "tuples"
    , "erl-maps"
    ]
  , repo = "https://github.com/purerl/purescript-profunctor-lenses.git"
  , version = "v8.0.0-erl1"
  }
, psci-support =
  { dependencies = [ "console", "effect", "prelude" ]
  , repo = "https://github.com/purescript/purescript-psci-support.git"
  , version = "v5.0.0"
  }
, quickcheck =
  { dependencies =
    [ "arrays"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "enums"
    , "exceptions"
    , "foldable-traversable"
    , "gen"
    , "identity"
    , "integers"
    , "lazy"
    , "lcg"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "record"
    , "strings"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purerl/purescript-quickcheck.git"
  , version = "v7.1.0-erl1"
  }
, quickcheck-laws =
  { dependencies =
    [ "arrays"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "identity"
    , "lists"
    , "maybe"
    , "newtype"
    , "prelude"
    , "quickcheck"
    , "tuples"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-quickcheck-laws"
  , version = "v6.0.1"
  }
, random =
  { dependencies = [ "effect", "integers", "math" ]
  , repo = "https://github.com/purerl/purescript-random.git"
  , version = "v5.0.0-erl1"
  }
, rationals =
  { dependencies = [ "prelude", "integers" ]
  , repo = "https://github.com/anttih/purescript-rationals.git"
  , version = "c883c972513380ae161d816ed42108acfe8cc8f6"
  }
, record =
  { dependencies = [ "functions", "typelevel-prelude", "unsafe-coerce" ]
  , repo = "https://github.com/purerl/purescript-record.git"
  , version = "v3.0.0-erl1"
  }
, record-prefix =
  { dependencies =
    [ "prelude", "heterogeneous", "console", "typelevel-prelude" ]
  , repo = "https://github.com/dariooddenino/purescript-record-prefix.git"
  , version = "v1.0.0"
  }
, refs =
  { dependencies = [ "effect", "prelude" ]
  , repo = "https://github.com/purerl/purescript-refs.git"
  , version = "v5.0.0-erl2"
  }
, routing-duplex =
  { dependencies =
    [ "arrays"
    , "control"
    , "either"
    , "js-uri"
    , "lazy"
    , "numbers"
    , "prelude"
    , "profunctor"
    , "record"
    , "strings"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/natefaubion/purescript-routing-duplex.git"
  , version = "v0.5.0"
  }
, safe-coerce =
  { dependencies = [ "unsafe-coerce" ]
  , repo = "https://github.com/purescript/purescript-safe-coerce.git"
  , version = "v1.0.0"
  }
, semirings =
  { dependencies = [ "foldable-traversable", "lists", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-semirings.git"
  , version = "v6.0.0"
  }
, sequences =
  { dependencies =
    [ "prelude"
    , "unsafe-coerce"
    , "partial"
    , "unfoldable"
    , "lazy"
    , "arrays"
    , "profunctor"
    , "maybe"
    , "tuples"
    , "newtype"
    ]
  , repo = "https://github.com/hdgarrood/purescript-sequences.git"
  , version = "v3.0.2"
  }
, simple-json =
  { dependencies =
    [ "exceptions"
    , "foreign"
    , "nullable"
    , "prelude"
    , "record"
    , "typelevel-prelude"
    , "variant"
    , "erl-lists"
    , "erl-maps"
    , "erl-kernel"
    ]
  , repo = "https://github.com/purerl/purescript-simple-json.git"
  , version = "baad5dd0d613df6fb2f054fd241d46e11a92e181"
  }
, simple-json-generics =
  { dependencies = [ "prelude", "simple-json" ]
  , repo = "https://github.com/justinwoo/purescript-simple-json-generics"
  , version = "v0.1.0"
  }
, strings =
  { dependencies =
    [ "arrays"
    , "control"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "gen"
    , "integers"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purerl/purescript-strings.git"
  , version = "v5.0.0-erl2"
  }
, tailrec =
  { dependencies =
    [ "bifunctors"
    , "effect"
    , "either"
    , "identity"
    , "maybe"
    , "partial"
    , "prelude"
    , "refs"
    ]
  , repo = "https://github.com/purerl/purescript-tailrec.git"
  , version = "v5.0.1-erl1"
  }
, these =
  { dependencies =
    [ "arrays", "gen", "lists", "quickcheck", "quickcheck-laws", "tuples" ]
  , repo = "https://github.com/purescript-contrib/purescript-these.git"
  , version = "v5.0.0"
  }
, transformers =
  { dependencies =
    [ "control"
    , "distributive"
    , "effect"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "identity"
    , "lazy"
    , "maybe"
    , "newtype"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-transformers.git"
  , version = "v5.2.0"
  }
, tuples =
  { dependencies = [ "control", "invariant", "prelude" ]
  , repo = "https://github.com/purescript/purescript-tuples.git"
  , version = "v6.0.1"
  }
, type-equality =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-type-equality.git"
  , version = "v4.0.0"
  }
, typelevel-prelude =
  { dependencies = [ "prelude", "type-equality" ]
  , repo = "https://github.com/purescript/purescript-typelevel-prelude.git"
  , version = "v6.0.0"
  }
, undefinable =
  { dependencies = [ "maybe", "functions" ]
  , repo = "https://github.com/purerl/purescript-undefinable.git"
  , version = "v4.0.0-erl1"
  }
, unfoldable =
  { dependencies =
    [ "foldable-traversable", "maybe", "partial", "prelude", "tuples" ]
  , repo = "https://github.com/purerl/purescript-unfoldable.git"
  , version = "v5.0.0-erl1"
  }
, unicode =
  { dependencies =
    [ "foldable-traversable", "maybe", "psci-support", "strings" ]
  , repo = "https://github.com/id3as/purescript-unicode"
  , version = "v5.0.0-erl1"
  }
, unsafe-coerce =
  { dependencies = [] : List Text
  , repo = "https://github.com/purerl/purescript-unsafe-coerce.git"
  , version = "v5.0.0-erl1"
  }
, unsafe-reference =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purerl/purescript-unsafe-reference.git"
  , version = "v4.0.0-erl1"
  }
, uri =
  { dependencies =
    [ "arrays"
    , "integers"
    , "js-uri"
    , "numbers"
    , "parsing"
    , "prelude"
    , "profunctor-lenses"
    , "these"
    , "transformers"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-uri"
  , version = "v8.0.1"
  }
, validation =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "either"
    , "foldable-traversable"
    , "newtype"
    , "prelude"
    ]
  , repo = "https://github.com/purescript/purescript-validation.git"
  , version = "v5.0.0"
  }
, variant =
  { dependencies =
    [ "enums"
    , "lists"
    , "maybe"
    , "partial"
    , "prelude"
    , "record"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/natefaubion/purescript-variant.git"
  , version = "v7.0.3"
  }
}
