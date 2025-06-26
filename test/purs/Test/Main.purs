module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Test.Spec.Colyseus.Client (spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (TestRunConfig,defaultConfig)

main ∷ Effect Unit
main = runSpecAndExitProcess' 
  {defaultConfig : testConfig, parseCLIOptions: true } 
  [ consoleReporter ] 
  spec
  where
  testConfig :: TestRunConfig
  testConfig =
    defaultConfig 
      { failFast= false
      , filter = Nothing
      , onlyFailures= false
      , timeout= Just $ Milliseconds $ slowTimeLimitInMs * 5.0
      }

  slowTimeLimitInMs ∷ Number
  slowTimeLimitInMs = 1000.0

