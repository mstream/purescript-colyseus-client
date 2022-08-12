module Main (main) where

import Prelude

import Chat as Chat
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI Chat.component unit body

