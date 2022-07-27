{ dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-core"
  , "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foreign-object"
  , "functions"
  , "lists"
  , "maybe"
  , "monad-loops"
  , "node-buffer"
  , "node-child-process"
  , "node-streams"
  , "prelude"
  , "refs"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, license = "MIT"
, name = "colyseus-client"
, packages = ./packages.dhall
, repository = "https://github.com/mstream/purescript-colyseus-client"
, sources = [ "src/purs/**/*.purs", "test/purs/**/*.purs" ]
}
