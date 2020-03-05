{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-parsing"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "unicode"
  , "assert"
  , "data-default"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/[!.]*.purs", "test/**/[!.]*.purs" ]
}
