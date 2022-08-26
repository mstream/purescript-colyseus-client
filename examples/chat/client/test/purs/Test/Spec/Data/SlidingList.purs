module Test.Spec.Data.SlidingList (spec) where

import Prelude

import Data.List as List
import Data.Maybe (Maybe(..))
import Data.SlidingList (Index(..), SlidingList)
import Data.SlidingList as SlidingList
import Data.Tuple.Nested ((/\))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Data.SlidingList" do
  describe "toList" do
    it "preserves the right order" $ quickCheck \x1 x2 x3 →
      let
        (xs ∷ SlidingList String) =
          SlidingList.fromFoldable [ x1, x2, x3 ]

        actual = SlidingList.toList xs
        expected = List.fromFoldable [ x1, x2, x3 ]
      in
        actual === expected

  describe "toIndexedList" do
    it "preserves the right order and indexes correctly"
      $ quickCheck \x1 x2 x3 →
          let
            (xs ∷ SlidingList String) =
              SlidingList.fromFoldable [ x1, x2, x3 ]

            actual = SlidingList.toIndexedList xs

            expected = List.fromFoldable
              [ AtCursor /\ x1
              , AfterCursor /\ x2
              , AfterCursor /\ x3
              ]
          in
            actual === expected

  describe "slideForwards" do
    it "slides by one" $ quickCheck \x1 x2 x3 →
      let
        (xs ∷ SlidingList String) =
          SlidingList.fromFoldable [ x1, x2, x3 ]

        actual =
          SlidingList.toIndexedList <$> SlidingList.slideForwards xs

        expected =
          Just $ List.fromFoldable
            [ BeforeCursor /\ x1
            , AtCursor /\ x2
            , AfterCursor /\ x3
            ]

      in
        actual === expected

    it "slides by two" $ quickCheck \x1 x2 x3 →
      let
        (xs ∷ SlidingList String) =
          SlidingList.fromFoldable [ x1, x2, x3 ]

        actual =
          SlidingList.toIndexedList <$>
            (SlidingList.slideForwards <=< SlidingList.slideForwards) xs

        expected =
          Just $ List.fromFoldable
            [ BeforeCursor /\ x1
            , BeforeCursor /\ x2
            , AtCursor /\ x3
            ]

      in
        actual === expected
