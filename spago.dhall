{ name = "shopping-cart"
, dependencies =
  [ "aff"
  , "aff-retry"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "httpure"
  , "identity"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "monad-logger"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "postgresql-client"
  , "prelude"
  , "psci-support"
  , "refinery"
  , "selda"
  , "simple-jwt"
  , "strings"
  , "test-unit"
  , "transformers"
  , "uuid"
  , "variant"
  , "node-bcryptjs"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
