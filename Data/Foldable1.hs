module Data.Foldable1 (Foldable1 (..), intercalate, foldrM1, foldlM1) where

import Prelude hiding (head, tail, init, last, scanl1, scanr1, foldl1, foldr1)
import Control.Applicative.Backwards
import Data.Foldable (foldl', foldlM)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.List.NonEmpty (NonEmpty (..), uncons)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..), Dual (..), Max (..), Min (..), First (..), Last (..))
import Util ((&))

class Foldable f => Foldable1 f where
    {-# MINIMAL foldMap1 | toNonEmpty #-}

    fold1 :: Semigroup a => f a -> a
    fold1 = foldMap1 id

    foldMap1 :: Semigroup b => (a -> b) -> f a -> b
    foldMap1 f = sconcat . fmap f . toNonEmpty

    foldr1, foldl1, foldr1', foldl1' :: (a -> a -> a) -> f a -> a
    foldr1 f = NE.head . NE.scanr1 f . toNonEmpty
    foldl1 f = NE.last . NE.scanl1 f . toNonEmpty
    foldl1' f = toNonEmpty & \ (a:|as) -> foldl' f a as
    foldr1' f = toNonEmpty & go
      where go = uncons & \ case (a, Nothing) -> a
                                 (a, Just as) -> a `f` go as

    toNonEmpty :: f a -> NonEmpty a
    toNonEmpty = foldMap1 pure

    maximumBy, minimumBy :: (a -> a -> Ordering) -> f a -> a
    maximumBy cmp = foldr1 max'
      where
        max' a b | GT <- cmp a b = a | otherwise = b
    minimumBy cmp = foldr1 min'
      where
        min' a b | LT <- cmp a b = b | otherwise = a

    maximum, minimum :: Ord a => f a -> a
    maximum = getMax . foldMap1 Max
    minimum = getMin . foldMap1 Min

    head, last :: f a -> a
    head = getFirst . foldMap1 First
    last = getLast . foldMap1 Last

intercalate :: (Foldable1 f, Semigroup a) => a -> f a -> a
intercalate a = sconcat . NE.intersperse a . toNonEmpty

foldrM1, foldlM1 :: (Foldable1 f, Monad m) => (a -> a -> m a) -> f a -> m a
foldrM1 f = go . toNonEmpty
  where go (a:|[])   = pure a
        go (a:|b:bs) = f a =<< go (b:|bs)
foldlM1 f = toNonEmpty & \ (a:|as) -> foldlM f a as

instance Foldable1 Identity where
    foldMap1 f = f . runIdentity

instance Foldable1 NonEmpty where
    toNonEmpty = id

instance Foldable1 ((,) a) where
    foldMap1 f = f . snd

deriving instance (Foldable1 f) => Foldable1 (Backwards f)

instance (Foldable1 f) => Foldable1 (Reverse f) where
    foldMap1 f (Reverse as) = getDual $ foldMap1 (Dual . f) as

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = (foldMap1 . foldMap1) f . getCompose

instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
    foldMap1 f (Pair as bs) = foldMap1 f as <> foldMap1 f bs

instance (Foldable1 f, Foldable1 g) => Foldable1 (Sum f g) where
    foldMap1 f (InL as) = foldMap1 f as
    foldMap1 f (InR bs) = foldMap1 f bs
