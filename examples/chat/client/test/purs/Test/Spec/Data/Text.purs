module Test.Spec.Data.Text (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List as List
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Text (Text(..), TextSegment(..), text)
import Partial.Unsafe (unsafePartial)
import StringParser (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Data.Text" do
  describe "text" do
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
          Right $ Text $ List.fromFoldable
            [ PlainText $ unsafeNonEmptyString "abc" ]
      actual `shouldEqual` expected
    it "interprets a single colon as a plain text" do
      let
        actual = runParser text ":"
        expected =
          Right $ Text $ List.fromFoldable
            [ PlainText $ unsafeNonEmptyString ":" ]
      actual `shouldEqual` expected
    it "interprets a single address sign as a plain text" do
      let
        actual = runParser text "@"
        expected =
          Right $ Text $ List.fromFoldable
            [ PlainText $ unsafeNonEmptyString "@" ]
      actual `shouldEqual` expected
    it "parses a single user reference segment" do
      let
        actual = runParser text "@abc"
        expected =
          Right $ Text $ List.fromFoldable
            [ UserReference $ unsafeNonEmptyString "abc" ]
      actual `shouldEqual` expected
    it "parses a single pictogram segment" do
      let
        actual = runParser text ":abc"
        expected =
          Right $ Text $ List.fromFoldable
            [ Pictogram $ unsafeNonEmptyString "abc" ]
      actual `shouldEqual` expected
    it "parses multiple segments" do
      let
        actual = runParser text "123@abc 456\n789:def:ghi 0"
        expected = Right $ Text $ List.fromFoldable
          [ PlainText $ unsafeNonEmptyString "123"
          , UserReference $ unsafeNonEmptyString "abc"
          , PlainText $ unsafeNonEmptyString " 456\n789"
          , Pictogram $ unsafeNonEmptyString "def"
          , Pictogram $ unsafeNonEmptyString "ghi"
          , PlainText $ unsafeNonEmptyString " 0"
          ]
      actual `shouldEqual` expected

unsafeNonEmptyString ∷ String → NonEmptyString
unsafeNonEmptyString = unsafePartial NEString.unsafeFromString
