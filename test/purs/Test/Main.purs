module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Colyseus.Client (spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpecT)

main :: Effect Unit
main = launchAff_ do
  resultAff <- runSpecT config [ consoleReporter ] do
    spec
  void resultAff
  where
  config =
    { exit: true
    , slow: Milliseconds slowTimeLimitInMs
    , timeout: Just $ Milliseconds $ slowTimeLimitInMs * 10.0
    }

  slowTimeLimitInMs ∷ Number
  slowTimeLimitInMs = 1000.0

