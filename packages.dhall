let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let overrides =
      { aff-retry =
              upstream.aff-retry
          //  { repo = "https://github.com/Nimor111/purescript-aff-retry.git"
              , version = "2f36dd0ecba206a24a5f03f754187c99c6c45b1b"
              }
      }

let additions =
      { polyform =
        { dependencies =
          [ "arrays"
          , "bifunctors"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "functors"
          , "identity"
          , "invariant"
          , "lists"
          , "maybe"
          , "newtype"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-laws"
          , "record"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "validation"
          , "variant"
          ]
        , repo = "https://github.com/purescript-polyform/polyform.git"
        , version = "v0.9.0"
        }
      , polyform-batteries-core =
        { dependencies =
          [ "arrays"
          , "decimals"
          , "effect"
          , "enums"
          , "integers"
          , "lazy"
          , "maybe"
          , "numbers"
          , "partial"
          , "polyform"
          , "prelude"
          , "psci-support"
          , "quickcheck"
          , "strings"
          , "test-unit"
          , "typelevel-prelude"
          , "validation"
          , "variant"
          ]
        , repo = "https://github.com/purescript-polyform/batteries-core.git"
        , version = "v0.2.0"
        }
      , polyform-batteries-env =
        { dependencies =
          [ "arrays"
          , "identity"
          , "maybe"
          , "ordered-collections"
          , "polyform"
          , "polyform-batteries-core"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/purescript-polyform/batteries-env.git"
        , version = "v0.1.0"
        }
      , postgresql-client =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "arrays"
          , "assert"
          , "bifunctors"
          , "bytestrings"
          , "datetime"
          , "decimals"
          , "dotenv"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-generic"
          , "foreign-object"
          , "identity"
          , "integers"
          , "js-date"
          , "lists"
          , "math"
          , "maybe"
          , "newtype"
          , "node-process"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "polyform"
          , "polyform-batteries-core"
          , "polyform-batteries-env"
          , "prelude"
          , "psci-support"
          , "string-parsers"
          , "strings"
          , "test-unit"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "validation"
          ]
        , repo = "https://github.com/rightfold/purescript-postgresql-client.git"
        , version = "v3.3.0"
        }
      , selda =
        { dependencies =
          [ "console"
          , "exists"
          , "heterogeneous"
          , "lists"
          , "node-sqlite3"
          , "postgresql-client"
          , "prelude"
          , "simple-json"
          , "strings"
          , "test-unit"
          , "transformers"
          , "variant"
          , "prettyprinter"
          , "dodo-printer"
          ]
        , repo = "https://github.com/Kamirus/purescript-selda.git"
        , version = "7e746ef8d6bd32184bb80315eb8ad8209266da34"
        }
      , prettyprinter =
        { dependencies =
          [ "prelude", "unfoldable", "random", "ansi", "console" ]
        , repo = "https://github.com/Kamirus/purescript-prettyprinter.git"
        , version = "686e9d02c3916a85c6ce1c499be5528a7102b533"
        }
      , refinery =
        { dependencies =
          [ "console"
          , "effect"
          , "either"
          , "psci-support"
          , "strings"
          , "these"
          , "typelevel"
          ]
        , repo = "https://github.com/Nimor111/purescript-refinery.git"
        , version = "0b7f8f3c425568b00aaef8fabadfba49e1c2ab68"
        }
      , node-bcryptjs =
        { dependencies =
          [ "aff"
          , "console"
          , "effect"
          , "free"
          , "newtype"
          , "prelude"
          , "psci-support"
          , "test-unit"
          ]
        , repo = "https://github.com/Nimor111/purescript-node-bcryptjs.git"
        , version = "8bfc8923c3204843d09b9469eb17d29d2fbf900a"
        }
      }

in  upstream // overrides // additions
