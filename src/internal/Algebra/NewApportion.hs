{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.NewApportion
    where

import Data.Coerce
    ( coerce )
import Data.Foldable
    ( foldrM )
import Data.List.NonEmpty.Extended
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( Ratio )
import Data.Semialign
    ( Semialign (..), Zip (..) )
import Data.Semigroup.Foldable
    ( Foldable1 (..) )
import Data.Set
    ( Set )
import Data.These
    ( These (..) )
import Numeric.Natural
    ( Natural )
import Roundable
    ( Fractional (..) )

import Prelude hiding
    ( Fractional, last, zip, zipWith )

import qualified Algebra.Apportion.Natural as Natural
import qualified Data.List as L
import qualified Data.List.NonEmpty.Extended as NE
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Partition
--------------------------------------------------------------------------------

data Partition a = Partition
    { remainder :: a
    , partition :: NonEmpty a
    }
    deriving stock (Eq, Foldable, Functor, Show)
    deriving anyclass Foldable1

instance Semialign Partition where
    alignWith f a0 a1 = Partition
        { remainder =           f $ These (remainder a0) (remainder a1)
        , partition = alignWith f         (partition a0) (partition a1)
        }

instance Zip Partition where
    zipWith f a0 a1 = Partition
        { remainder =         f (remainder a0) (remainder a1)
        , partition = zipWith f (partition a0) (partition a1)
        }

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, Semigroup a, Semigroup (Weight a)) => Apportion a where

    type Weight a

    apportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default apportion :: Monoid a
        => a -> NonEmpty (Weight a) -> Partition a
    apportion a as = case apportionMaybe a as of
        Nothing -> Partition a (mempty <$ as)
        Just bs -> Partition mempty bs

    apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default apportionMaybe :: Monoid a
        => a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       Partition b bs | b == mempty -> Just bs
       _ -> Nothing

apportionLaw_fold :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_fold a ws =
    fold1 (apportion a ws) == a

apportionLaw_length :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_length a ws =
    length (partition (apportion a ws)) == length ws

apportionLaw_maybe :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_maybe a ws =
    isJust (apportionMaybe a ws) == (fold1 (partition (apportion a ws)) == a)

--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------

class
    ( Apportion a
    , ExactApportion (Exact a)
    , Fractional a (Exact a)
    ) =>
    BalancedApportion a
  where
    type Exact a

    balancedApportionOrder
        :: Exact a -> Exact a -> Bool

    balancedApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default balancedApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    balancedApportion = apportion

    balancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default balancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    balancedApportionMaybe = apportionMaybe

balancedApportionIsBalanced
    :: forall a. BalancedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Bool
balancedApportionIsBalanced = undefined {-a ws = (&&)
    (and $ zipWith (balancedApportionOrder @a) lowerBound' rounded)
    (and $ zipWith (balancedApportionOrder @a) rounded upperBound')
  where
    lowerBound' = exact <&> lowerBound
    upperBound' = exact <&> upperBound
    rounded = balancedApportionToRounded <$> apportion a ws
    exact = exactApportion
        (balancedApportionToExact a)
        (balancedApportionToExactWeight @a <$> ws)
-}

balancedApportionLaw_balanced
    :: forall a. BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_balanced =
    balancedApportionIsBalanced

balancedApportionLaw_identity
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity a ws =
    balancedApportion a ws == apportion a ws

balancedApportionLaw_identity_maybe
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity_maybe a ws =
    balancedApportionMaybe a ws == apportionMaybe a ws

--------------------------------------------------------------------------------
-- ExactApportion
--------------------------------------------------------------------------------

class Apportion a => ExactApportion a where

    exactApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default exactApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    exactApportion = apportion

    exactApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default exactApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    exactApportionMaybe = apportionMaybe

exactApportionLaw_identity
    :: ExactApportion a => a -> NonEmpty (Weight a) -> Bool
exactApportionLaw_identity a ws =
    exactApportion a ws == apportion a ws

exactApportionLaw_identity_maybe
    :: ExactApportion a => a -> NonEmpty (Weight a) -> Bool
exactApportionLaw_identity_maybe a ws =
    exactApportionMaybe a ws == apportionMaybe a ws

exactApportionLaw_folds
    :: ExactApportion a => a -> NonEmpty (Weight a) -> Bool
exactApportionLaw_folds a ws =
    folds (partition (apportion a ws)) == (partition . apportion a <$> folds ws)

--------------------------------------------------------------------------------
-- Instances: Sum (Ratio Natural)
--------------------------------------------------------------------------------

instance Apportion (Sum (Ratio Natural)) where

    type Weight (Sum (Ratio Natural)) = Sum (Ratio Natural)

    apportionMaybe a ws
        | weightSum == mempty = Nothing
        | otherwise = Just (mkPortion <$> ws)
      where
        weightSum = fold1 ws
        mkPortion w = Sum (getSum a * getSum w / getSum weightSum)

instance ExactApportion (Sum (Ratio Natural))

--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------

instance Apportion (Sum Natural) where
    type Weight (Sum Natural) = Sum Natural
    apportionMaybe = coerce Natural.apportion

{-instance BalancedApportion (Sum Natural) where
    balancedApportionOrder = (<=)
-}
--------------------------------------------------------------------------------
-- Instances: Sublist
--------------------------------------------------------------------------------

newtype Sublist a = Sublist
    {getSublist :: [a]}
    deriving newtype (Eq, Monoid, Semigroup, Show)

newtype SublistLength = SublistLength
    {getSublistLength :: Natural}
    deriving (Eq, Ord, Semigroup) via Sum Natural

newtype SublistLengthIdeal = SublistLengthIdeal
    {getSublistLengthIdeal :: Ratio Natural}
    deriving (Apportion, Eq, ExactApportion, Semigroup)
        via Sum (Ratio Natural)

instance Eq a => Apportion (Sublist a) where
    type Weight (Sublist a) = SublistLength
    apportionMaybe (Sublist as) ws = do
        chunkLengths <- maybeChunkLengths
        Just $ NE.unfoldr makeChunk (chunkLengths, Sublist as)
      where
        maybeChunkLengths :: Maybe (NonEmpty Int)
        maybeChunkLengths =
            fmap (fromIntegral @Natural @Int . getSum) <$>
            apportionMaybe
                (Sum $ fromIntegral @Int @Natural $ length as)
                (fromIntegral . getSublistLength <$> ws)

        makeChunk
            :: (NonEmpty Int, Sublist a)
            -> (Sublist a, Maybe (NonEmpty Int, Sublist a))
        makeChunk (c :| mcs, Sublist bs) = case NE.nonEmpty mcs of
            Just cs -> (Sublist prefix, Just (cs, Sublist suffix))
            Nothing -> (Sublist bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

{-instance Ord a => BalancedApportion (Sublist a) where
    balancedApportionOrder = (<=)
-}
--------------------------------------------------------------------------------
-- Instances: Subset
--------------------------------------------------------------------------------

newtype Subset a = Subset
    {getSubset :: Set a}
    deriving newtype (Eq, Monoid, Semigroup, Show)

newtype SubsetSize = SubsetSize
    {getSubsetSize :: Natural}
    deriving (Eq, Ord, Semigroup) via Sum Natural

newtype SubsetSizeIdeal = SubsetSizeIdeal
    {getSubsetSizeIdeal :: Ratio Natural}
    deriving (Apportion, Eq, ExactApportion, Semigroup)
        via Sum (Ratio Natural)

instance Ord a => Apportion (Subset a) where
    type Weight (Subset a) = SubsetSize
    apportion as ws = Subset . Set.fromList . getSublist <$> apportion
        (Sublist $ Set.toList $ getSubset as)
        (SublistLength . getSubsetSize <$> ws)
{-
instance Ord a => BalancedApportion (Subset a) where
    balancedApportionOrder = (<=)
-}
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

folds :: Semigroup a => NonEmpty a -> [NonEmpty a]
folds ws = case NE.splitLast ws of
    Just (prefix, last) ->
        foldrM acc (NE.singleton last) prefix
    Nothing ->
        [ws]
  where
    acc a (x :| xs) = [a <> x :| xs, a :| x : xs]

zipAll :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAll f xs ys = all (uncurry f) (zip xs ys)

zipAny :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAny f xs ys = any (uncurry f) (zip xs ys)
