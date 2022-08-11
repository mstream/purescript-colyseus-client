{ name = "colyseus-client-chat-example"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "colyseus-client"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "spec"
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "transformers"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  ]
, packages =
    ../../../packages.dhall
  with colyseus-client = ../../../spago.dhall as Location
, sources = [ "src/purs/**/*.purs", "test/purs/**/*.purs" ]
}
