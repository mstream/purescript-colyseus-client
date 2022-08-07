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
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-html"
  , "web-uievents"
  ]
, packages =
    ../../../packages.dhall
  with colyseus-client = ../../../spago.dhall as Location
, sources = [ "src/purs/**/*.purs" ]
}
