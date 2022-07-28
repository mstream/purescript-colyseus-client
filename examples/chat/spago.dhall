{ name = "colyseus-client-chat-example"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arrays"
  , "colyseus-client"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "lists"
  , "maybe"
  , "prelude"
  , "transformers"
  ]
, packages =
    ../../packages.dhall
  with colyseus-client = ../../spago.dhall as Location
, sources = [ "src/purs/**/*.purs" ]
}
