{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Prelude hiding (foldr1, foldl1, head, last, maximum, minimum)

import Control.DeepSeq    (NFData (..))
import Criterion.Main
import Data.Foldable      (Foldable)
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup     (Min (..))
import Data.Tree          (Tree (..))

main :: IO ()
main = defaultMain
    -- NonEmpty left folds
  [ b "NonEmpty" list
  , b "NonEmpty-foldMap1" (WithFoldMap1 list)
  , b "NonEmpty-toNonEmpty" (WithToNonEmpty list)

    -- Trees
  , b "Tree" tree
  , b "Tree-foldMap1" (WithFoldMap1 tree)
  , b "Tree-toNonEmpty" (WithToNonEmpty tree)
  ]
  where
    list :: NonEmpty Int
    list = 1 :| take 10000000 [2 .. ]

    tree :: Tree Int
    tree = go 7 0 where
        go :: Int -> Int -> Tree Int
        go n x
          | n <= 0    = Node x []
          | otherwise = Node x [ go (pred n) (x * 10 + x') | x' <- [0 .. 9] ]

b :: (NFData (f b), Foldable1 f, Ord b) => [Char] -> f b -> Benchmark
b name x = env (pure x) $ \ x -> bgroup name
  [ bench name $ whnf f x
  | (name, f) <-
      [ ("head", head)
      , ("last", last)
      , ("minimum", minimum)
      , ("foldMap1 Min", getMin . foldMap1 Min)
      , ("foldMap1' Min", getMin . foldMap1' Min)
      , ("foldr1 min", foldr1 min)
      , ("foldl1 min", foldl1 min)
      , ("foldr1' min", foldr1' min)
      , ("foldl1' min", foldl1' min)
      ]
  ]

instance Foldable1 Tree where
    foldMap1 f = go
      where
        go = \ case
            Node x [] -> f x
            Node x (y:ys) -> f x <> (foldMap1 . foldMap1) f (y :| ys)

    head (Node x _) = x

-------------------------------------------------------------------------------
-- Variants
-------------------------------------------------------------------------------

newtype WithFoldMap1 f a = WithFoldMap1 { unWithFoldMap1 :: f a }
  deriving (Foldable, NFData)

instance Foldable1 f => Foldable1 (WithFoldMap1 f) where
    foldMap1 f = foldMap1 f . unWithFoldMap1

newtype WithToNonEmpty f a = WithToNonEmpty { unWithToNonEmpty :: f a }
  deriving (Foldable, NFData)

instance Foldable1 f => Foldable1 (WithToNonEmpty f) where
    toNonEmpty = toNonEmpty . unWithToNonEmpty
