module Data.SlidingList
  ( Index(..)
  , SlidingList
  , cursorIndex
  , currentItem
  , empty
  , fromFoldable
  , length
  , make
  , null
  , precedingItems
  , slideBackwards
  , slideForwards
  , succeedingItems
  , toIndexedList
  , toList
  , toUnfoldable
  ) where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr)
import Test.QuickCheck (class Arbitrary)

newtype SlidingList a =
  SlidingList
    { itemAtCursor ∷ Maybe a
    , itemsAfterCursor ∷ List a
    , itemsBeforeCursor ∷ List a
    }

derive newtype instance Arbitrary a ⇒ Arbitrary (SlidingList a)
derive newtype instance Eq a ⇒ Eq (SlidingList a)
derive newtype instance Show a ⇒ Show (SlidingList a)

data Index = AfterCursor | AtCursor | BeforeCursor

derive instance Generic Index _
derive instance Eq Index

instance Show Index where
  show = genericShow

instance Functor SlidingList where
  map
    f
    ( SlidingList
        { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }
    ) =
    SlidingList
      { itemAtCursor: f <$> itemAtCursor
      , itemsAfterCursor: f <$> itemsAfterCursor
      , itemsBeforeCursor: f <$> itemsBeforeCursor
      }

instance FunctorWithIndex Index SlidingList where
  mapWithIndex
    f
    ( SlidingList
        { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }
    ) =
    SlidingList
      { itemAtCursor: f AtCursor <$> itemAtCursor
      , itemsAfterCursor: f AfterCursor <$> itemsAfterCursor
      , itemsBeforeCursor: f BeforeCursor <$> itemsBeforeCursor
      }

instance Foldable SlidingList where
  foldl f seed xs = foldl f seed $ toList xs
  foldr f seed xs = foldr f seed $ toList xs
  foldMap f xs = foldMap f $ toList xs

instance FoldableWithIndex Index SlidingList where
  foldlWithIndex f seed xs =
    foldl (\acc (i /\ x) → f i acc x) seed $ toIndexedList xs

  foldrWithIndex f seed xs =
    foldr (\(i /\ x) acc → f i x acc) seed $ toIndexedList xs

  foldMapWithIndex f xs =
    foldMap (\(i /\ x) → f i x) $ toIndexedList xs

toList ∷ SlidingList ~> List
toList
  (SlidingList { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }) =
  List.reverse itemsBeforeCursor
    <> maybe Nil List.singleton itemAtCursor
    <> itemsAfterCursor

toIndexedList ∷ ∀ a. SlidingList a → List (Index /\ a)
toIndexedList
  (SlidingList { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }) =
  List.reverse (Tuple BeforeCursor <$> itemsBeforeCursor)
    <> (Tuple AtCursor <$> maybe Nil List.singleton itemAtCursor)
    <> (Tuple AfterCursor <$> itemsAfterCursor)

toUnfoldable ∷ ∀ f. Unfoldable f ⇒ SlidingList ~> f
toUnfoldable = unfoldr
  \(SlidingList { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }) →
    case itemsBeforeCursor of
      x : xs →
        Just $ x /\ SlidingList
          { itemAtCursor, itemsAfterCursor, itemsBeforeCursor: xs }

      Nil →
        case itemAtCursor of
          Just y →
            Just $ y /\ SlidingList
              { itemAtCursor: Nothing
              , itemsAfterCursor
              , itemsBeforeCursor
              }

          Nothing →
            case itemsAfterCursor of
              z : zs →
                Just $ z /\ SlidingList
                  { itemAtCursor
                  , itemsAfterCursor: zs
                  , itemsBeforeCursor
                  }

              Nil →
                Nothing

instance Unfoldable1 SlidingList where
  unfoldr1 ∷ ∀ a b. (b → a /\ Maybe b) → b → SlidingList a
  unfoldr1 f seed =
    let
      x /\ mbNextSeed = f seed
    in
      go (make $ List.singleton x) mbNextSeed
    where
    go ∷ SlidingList a → Maybe b → SlidingList a
    go acc mbNextSeed = case mbNextSeed of
      Just nextSeed →
        let
          x /\ mbNextSeed = f nextSeed
        in
          go (acc `snoc` x) mbNextSeed
      Nothing → acc

instance Unfoldable SlidingList where
  unfoldr ∷ ∀ a b. (b → Maybe (a /\ b)) → b → SlidingList a
  unfoldr f seed = go (make Nil) (f seed)
    where
    go ∷ SlidingList a → Maybe (a /\ b) → SlidingList a
    go acc mbItemAndNextSeed = case mbItemAndNextSeed of
      Just (x /\ nextSeed) → go (acc `snoc` x) (f nextSeed)
      Nothing → acc

snoc ∷ ∀ a. SlidingList a → a → SlidingList a
snoc xs x = make $ toList xs `List.snoc` x

make ∷ List ~> SlidingList
make xs = SlidingList case List.uncons xs of
  Just { head, tail } →
    { itemAtCursor: Just head
    , itemsAfterCursor: tail
    , itemsBeforeCursor: Nil
    }
  Nothing →
    { itemAtCursor: Nothing
    , itemsAfterCursor: Nil
    , itemsBeforeCursor: Nil
    }

empty ∷ ∀ a. SlidingList a
empty = make Nil

fromFoldable ∷ ∀ a f. Foldable f ⇒ f a → SlidingList a
fromFoldable = make <<< List.fromFoldable

length ∷ ∀ a. SlidingList a → Int
length
  (SlidingList { itemsAfterCursor, itemAtCursor, itemsBeforeCursor }) =
  (if isJust itemAtCursor then 1 else 0)
    + List.length itemsBeforeCursor
    + List.length itemsAfterCursor

null ∷ ∀ a. SlidingList a → Boolean
null = length >>> eq 0

cursorIndex ∷ ∀ a. SlidingList a → Int
cursorIndex (SlidingList { itemsBeforeCursor }) =
  List.length itemsBeforeCursor

currentItem ∷ ∀ a. SlidingList a → Maybe a
currentItem (SlidingList { itemAtCursor }) = itemAtCursor

precedingItems ∷ ∀ a. SlidingList a → List a
precedingItems (SlidingList { itemsBeforeCursor }) =
  List.reverse itemsBeforeCursor

succeedingItems ∷ ∀ a. SlidingList a → List a
succeedingItems (SlidingList { itemsAfterCursor }) =
  itemsAfterCursor

slideBackwards
  ∷ ∀ a. SlidingList a → Maybe (SlidingList a)
slideBackwards
  (SlidingList { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }) =
  case itemAtCursor, List.uncons itemsBeforeCursor of
    Just x, Just { head, tail } → Just
      $ SlidingList
          { itemAtCursor: Just head
          , itemsAfterCursor: x : itemsAfterCursor
          , itemsBeforeCursor: tail
          }
    _, _ → Nothing

slideForwards
  ∷ ∀ a. SlidingList a → Maybe (SlidingList a)
slideForwards
  (SlidingList { itemAtCursor, itemsAfterCursor, itemsBeforeCursor }) =
  case itemAtCursor, List.uncons itemsAfterCursor of
    Just x, Just { head, tail } → Just
      $ SlidingList
          { itemAtCursor: Just head
          , itemsAfterCursor: tail
          , itemsBeforeCursor: x : itemsBeforeCursor
          }
    _, _ → Nothing
