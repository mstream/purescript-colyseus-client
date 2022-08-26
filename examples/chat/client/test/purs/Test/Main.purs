module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Data as Data
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (Config, runSpec')

main ∷ Effect Unit
main = launchAff_ $ runSpec' testConfig [ consoleReporter ]
  do
    Data.spec
  where
  testConfig ∷ Config
  testConfig =
    { exit: true
    , slow: Milliseconds slowTimeLimitInMs
    , timeout: Just $ Milliseconds $ slowTimeLimitInMs * 5.0
    }

  slowTimeLimitInMs ∷ Number
  slowTimeLimitInMs = 1000.0

