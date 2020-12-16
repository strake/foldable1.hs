module Data.Foldable1 (Foldable1 (..), intercalate, foldrM1, foldlM1) where

import Prelude hiding (head, tail, init, last, scanl1, scanr1, foldl1, foldr1, maximum, minimum)
import Control.Applicative.Backwards
import Data.Foldable (foldl', foldlM)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..), Dual (..), Max (..), Min (..), First (..), Last (..))
import Util ((&))

class Foldable f => Foldable1 f where
    {-# MINIMAL foldMap1 | toNonEmpty #-}

    fold1 :: Semigroup a => f a -> a
    fold1 = foldMap1 id

    foldMap1, foldMap1' :: Semigroup b => (a -> b) -> f a -> b
    foldMap1 f = sconcat . fmap f . toNonEmpty
    foldMap1' f = foldMap1' f . toNonEmpty

    foldr1, foldl1, foldr1', foldl1' :: (a -> a -> a) -> f a -> a
    foldr1 f = foldr1 f . toNonEmpty
    foldl1 f = foldl1 f . toNonEmpty
    foldl1' f = foldl1' f . toNonEmpty
    foldr1' f = foldl1' (flip f) . Reverse

    toNonEmpty :: f a -> NonEmpty a
    toNonEmpty = runNonEmptyDList . foldMap1 singleton

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

intercalate1 :: (Foldable1 f, Semigroup a) => a -> f a -> a
intercalate1 a = sconcat . NE.intersperse a . toNonEmpty

intercalate :: (Foldable1 f, Semigroup a) => a -> f a -> a
intercalate = intercalate1
{-# DEPRECATED intercalate "Rather use `intercalate1`" #-}

foldrM1, foldlM1 :: (Foldable1 f, Monad m) => (a -> a -> m a) -> f a -> m a
foldrM1 f = go . toNonEmpty
  where go (a:|[])   = pure a
        go (a:|b:bs) = f a =<< go (b:|bs)
foldlM1 f = toNonEmpty & \ (a:|as) -> foldlM f a as

instance Foldable1 Identity where
    foldMap1 f = f . runIdentity

instance Foldable1 NonEmpty where
    toNonEmpty = id
    foldMap1' f (a:|as) = foldl' ((. f) . (<>)) (f a) as
    foldr1 f = go <$> NE.tail <*> NE.head where
        go = \ case
            [] -> id
            a:as -> f `flip` go as a
    foldl1 f (a:|as) = foldl f a as
    foldl1' f (a:|as) = foldl' f a as
    head (a:|_) = a

instance Foldable1 ((,) a) where
    foldMap1 f = f . snd
    head = snd
    last = snd

deriving instance (Foldable1 f) => Foldable1 (Backwards f)

instance (Foldable1 f) => Foldable1 (Reverse f) where
    foldMap1 f (Reverse as) = getDual $ foldMap1 (Dual . f) as

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = (foldMap1 . foldMap1) f . getCompose

instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
    foldMap1 f (Pair as bs) = foldMap1 f as <> foldMap1 f bs
    head (Pair as _) = head as
    last (Pair _ bs) = last bs

instance (Foldable1 f, Foldable1 g) => Foldable1 (Sum f g) where
    fold1 = sumElim fold1 fold1
    foldMap1 f = sumElim (foldMap1 f) (foldMap1 f)
    foldr1 f = sumElim (foldr1 f) (foldr1 f)
    foldl1 f = sumElim (foldl1 f) (foldl1 f)
    foldr1' f = sumElim (foldr1' f) (foldr1' f)
    foldl1' f = sumElim (foldl1' f) (foldl1' f)
    toNonEmpty = sumElim toNonEmpty toNonEmpty
    maximumBy f = sumElim (maximumBy f) (maximumBy f)
    minimumBy f = sumElim (minimumBy f) (minimumBy f)
    maximum = sumElim maximum maximum
    minimum = sumElim minimum minimum
    head = sumElim head head
    last = sumElim last last

sumElim :: (f a -> b) -> (g a -> b) -> Sum f g a -> b
sumElim f g = \ case
    InL as -> f as
    InR bs -> g bs
{-# INLINE sumElim #-}

newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
    NEDL f <> NEDL g = NEDL (f . NE.toList . g)
    {-# INLINE (<>) #-}

singleton :: a -> NonEmptyDList a
singleton = NEDL . (:|)
{-# INLINE singleton #-}

runNonEmptyDList :: NonEmptyDList a -> NonEmpty a
runNonEmptyDList = ($ []) . unNEDL
{-# INLINE runNonEmptyDList #-}
