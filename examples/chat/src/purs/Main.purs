module Main where

import Prelude

import Effect (Effect)
import Chat as Chat
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Chat.component unit body

