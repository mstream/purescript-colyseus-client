module Test.Spec.Data.Text (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List as List
import Data.Text (Text(..), TextSegment(..), text)
import StringParser (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Data.Text" do
  describe "textSegment" do
    it "parses an empty string" do
      let
        actual = runParser text ""
        expected =
          Right $ Text $ List.fromFoldable []
      actual `shouldEqual` expected
    it "parses a single plain text segment" do
      let
        actual = runParser text "abc"
        expected =
          Right $ Text $ List.fromFoldable [ PlainText "abc" ]
      actual `shouldEqual` expected
    it "parses a single user reference segment" do
      let
        actual = runParser text "@abc"
        expected =
          Right $ Text $ List.fromFoldable [ UserReference "abc" ]
      actual `shouldEqual` expected
    it "parses multiple segments" do
      let
        actual = runParser text "123@abc 456\n789"
        expected = Right $ Text $ List.fromFoldable
          [ PlainText "123"
          , UserReference "abc"
          , PlainText " 456\n789"
          ]
      actual `shouldEqual` expected
