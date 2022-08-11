module Test.Spec.Data (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Spec.Data.Text as Text

spec ∷ Spec Unit
spec = describe "Data" do
  Text.spec
