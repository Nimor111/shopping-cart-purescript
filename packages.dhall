let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200123/packages.dhall
      sha256:687bb9a2d38f2026a89772c47390d02939340b01e31aaa22de9247eadd64af05

let overrides = 
  { aff-retry = 
      upstream.aff-retry // 
        { repo = "https://github.com/Nimor111/purescript-aff-retry.git"
        , version = "2f36dd0ecba206a24a5f03f754187c99c6c45b1b" 
        }
  }

let additions =
  { postgresql-client =
      { dependencies =
          [ "aff"
          , "arrays"
          , "argonaut"
          , "assert"
          , "bifunctors"
          , "bytestrings"
          , "console"
          , "datetime"
          , "decimals"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-generic"
          , "foreign-object"
          , "js-date"
          , "lists"
          , "maybe"
          , "newtype"
          , "nullable"
          , "prelude"
          , "psci-support"
          , "test-unit"
          , "transformers"
          , "tuples" ]
      , repo =
          "https://github.com/rightfold/purescript-postgresql-client.git"
      , version =
          "v3.0.3"
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
        , "prettyprinter" ]
    , repo = 
        "https://github.com/Kamirus/purescript-selda.git"
    , version = 
        "5994f9ffe702518d326d42d54c67e57fe906fc2e"
    }
  , prettyprinter = 
    { dependencies = 
        [ "prelude"
        , "unfoldable"
        , "random"
        , "ansi"
        , "console" ]
    , repo =
        "https://github.com/Kamirus/purescript-prettyprinter.git"
    , version = 
        "686e9d02c3916a85c6ce1c499be5528a7102b533"
    }
  , refinery =
    { dependencies = 
        [ "console"
        , "effect"
        , "either"
        , "generics-rep"
        , "psci-support"
        , "strings"
        , "these"
        , "typelevel" ]
    , repo = 
        "https://github.com/thought2/purescript-refinery.git"
    , version =
        "548d3b66b6c3b4ab06422c9dbf0544cfc0860ff2"
    }
  , node-bcrypt = 
    { dependencies = 
        [ "prelude"
        , "aff"
        , "newtype" ]
    , repo =
        "https://github.com/vlopmartin/purescript-node-bcrypt.git"
    , version =
        "54b9c8ae7161d8e75fc876436ba6ffdcae6fa9e3"
    }
  }

in  upstream // overrides // additions
