{ name = "colyseus-client-chat-example"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "colyseus-client"
  , "console"
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
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  ]
, packages =
    ../../../packages.dhall
  with colyseus-client = ../../../spago.dhall as Location
, sources = [ "src/purs/**/*.purs" ]
}
