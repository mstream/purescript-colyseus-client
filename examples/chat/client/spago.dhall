let colyseusClient = ../../../spago.dhall as Location

let searchTrie =
      { dependencies =
        [ "arrays"
        , "assert"
        , "bifunctors"
        , "console"
        , "effect"
        , "foldable-traversable"
        , "lists"
        , "ordered-collections"
        , "prelude"
        , "psci-support"
        , "strings"
        ]
      , repo = "https://github.com/klntsky/purescript-search-trie.git"
      , version = "v1.0.0"
      }

in  { name = "colyseus-client-chat-example"
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
      , "foreign"
      , "foreign-object"
      , "functions"
      , "halogen"
      , "halogen-subscriptions"
      , "integers"
      , "lists"
      , "maybe"
      , "now"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "quickcheck"
      , "search-trie"
      , "spec"
      , "spec-quickcheck"
      , "string-parsers"
      , "strings"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "unfoldable"
      , "web-dom"
      , "web-events"
      , "web-html"
      , "web-uievents"
      ]
    , packages =
        ../../../packages.dhall
      with colyseus-client = colyseusClient
      with search-trie = searchTrie
    , sources = [ "src/purs/**/*.purs", "test/purs/**/*.purs" ]
    }
