let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211109/packages.dhall sha256:e8d8d5b339f6d46d950da90037c6c38e8809f7e34f727373089ab82c080fc709

let overrides =
      { aff-retry =
          upstream.aff-retry // 
              { repo = "https://github.com/Nimor111/purescript-aff-retry.git"
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
      , node-bcrypt =
        { dependencies = [ "prelude", "aff", "newtype" ]
        , repo = "https://github.com/vlopmartin/purescript-node-bcrypt.git"
        , version = "54b9c8ae7161d8e75fc876436ba6ffdcae6fa9e3"

    }
}

in  upstream // overrides // additions
