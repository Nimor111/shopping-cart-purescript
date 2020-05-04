{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "shopping-cart"
, dependencies = [ 
    "console", 
    "effect", 
    "halogen", 
    "psci-support", 
    "httpure", 
    "postgresql-client", 
    "selda",
    "uuid",
    "ordered-collections",
    "aff-retry",
    "simple-json",
    "simple-jwt",
    "checked-exceptions",
    "refinery",
    "monad-logger"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
