{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.NewApportion
    where

import Algebra.ExactBounded
    ( ExactBounded (..) )
import Algebra.PartialOrd.Extended
    ( PartialOrd (..) )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( foldrM )
import Data.List.Fraction
    ( ListFraction )
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
import Data.These
    ( These (..) )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( last, zip, zipWith )

import qualified Algebra.Apportion.Natural as Natural
import qualified Data.List as L
import qualified Data.List.Fraction as LF
import qualified Data.List.NonEmpty.Extended as NE

--------------------------------------------------------------------------------
-- Apportionment
--------------------------------------------------------------------------------

data Apportionment a = Apportionment
    { remainder :: a
    , partition :: NonEmpty a
    }
    deriving stock (Eq, Foldable, Functor, Show)
    deriving anyclass Foldable1

instance Semialign Apportionment where
    alignWith f a0 a1 = Apportionment
        { remainder =           f $ These (remainder a0) (remainder a1)
        , partition = alignWith f         (partition a0) (partition a1)
        }

instance Zip Apportionment where
    zipWith f a0 a1 = Apportionment
        { remainder =         f (remainder a0) (remainder a1)
        , partition = zipWith f (partition a0) (partition a1)
        }

instance PartialOrd a => PartialOrd (Apportionment a) where
    leq a0 a1 = and $ zipWith leq a0 a1

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, Semigroup a, Semigroup (Weight a)) => Apportion a where

    type Weight a

    apportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    default apportion :: Monoid a
        => a -> NonEmpty (Weight a) -> Apportionment a
    apportion a as = case apportionMaybe a as of
        Nothing -> Apportionment a (mempty <$ as)
        Just bs -> Apportionment mempty bs

    apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default apportionMaybe :: Monoid a
        => a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       Apportionment b bs | b == mempty -> Just bs
       _ -> Nothing

    {-# MINIMAL apportion | apportionMaybe #-}

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
-- ExactApportion
--------------------------------------------------------------------------------

class Apportion a => ExactApportion a where

    exactApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    default exactApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
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
-- BoundedApportion
--------------------------------------------------------------------------------

class
    ( Apportion a
    , ExactApportion (Exact a)
    , ExactBounded (Exact a) a
    , ExactBounded (Weight (Exact a)) (Weight a)
    , PartialOrd a
    ) =>
    BoundedApportion a
  where
    type Exact a

    boundedApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    default boundedApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    boundedApportion = apportion

    boundedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default boundedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    boundedApportionMaybe = apportionMaybe

boundedApportionIsBounded
    :: forall a. BoundedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Bool
boundedApportionIsBounded a ws = (&&)
    (boundedApportionLowerBound a ws `leq` boundedApportion           a ws)
    (boundedApportion           a ws `leq` boundedApportionUpperBound a ws)

boundedApportionIsExact
    :: forall a. BoundedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Bool
boundedApportionIsExact a ws = (==)
    (boundedApportionLowerBound a ws)
    (boundedApportionUpperBound a ws)

boundedApportionLowerBound
    :: forall a. BoundedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Apportionment a
boundedApportionLowerBound a ws =
    toLowerBound <$> exactApportion (toExact a) (toExact <$> ws)

boundedApportionUpperBound
    :: forall a. BoundedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Apportionment a
boundedApportionUpperBound a ws =
    toUpperBound <$> exactApportion (toExact a) (toExact <$> ws)

boundedApportionLaw_identity
    :: BoundedApportion a => a -> NonEmpty (Weight a) -> Bool
boundedApportionLaw_identity a ws =
    boundedApportion a ws == apportion a ws

boundedApportionLaw_identity_maybe
    :: BoundedApportion a => a -> NonEmpty (Weight a) -> Bool
boundedApportionLaw_identity_maybe a ws =
    boundedApportionMaybe a ws == apportionMaybe a ws

boundedApportionLaw_isBounded
    :: forall a. BoundedApportion a => a -> NonEmpty (Weight a) -> Bool
boundedApportionLaw_isBounded =
    boundedApportionIsBounded

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------

type NaturalRatio = Ratio Natural
type NaturalRatioSum = Sum NaturalRatio
type NaturalSum = Sum Natural

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

newtype Size a = Size {getSize :: a}
    deriving stock (Eq, Show)

deriving via NaturalSum instance Semigroup (Size Natural)
deriving via NaturalSum instance Monoid    (Size Natural)

deriving via NaturalRatioSum instance Semigroup (Size NaturalRatio)
deriving via NaturalRatioSum instance Monoid    (Size NaturalRatio)

instance ExactBounded (Size NaturalRatio) (Size Natural) where
    toExact (Size n) = Size (toExact n)
    toLowerBound (Size r) = Size (toLowerBound r)
    toUpperBound (Size r) = Size (toUpperBound r)

--------------------------------------------------------------------------------
-- Instances: NaturalSum
--------------------------------------------------------------------------------

instance Apportion NaturalSum where
    type Weight NaturalSum = NaturalSum
    apportionMaybe = coerce Natural.apportion

instance BoundedApportion NaturalSum where
    type Exact NaturalSum = NaturalRatioSum

--------------------------------------------------------------------------------
-- Instances: NaturalRatioSum
--------------------------------------------------------------------------------

instance Apportion NaturalRatioSum where
    type Weight NaturalRatioSum = NaturalRatioSum
    apportionMaybe a ws
        | weightSum == mempty = Nothing
        | otherwise = Just (mkPortion <$> ws)
      where
        weightSum = fold1 ws
        mkPortion w = Sum (getSum a * getSum w / getSum weightSum)

instance ExactApportion NaturalRatioSum

--------------------------------------------------------------------------------
-- Instances: []
--------------------------------------------------------------------------------

-- make a HasLength type, with the ability to splitAt, and a variable length
-- type.

deriving newtype instance Eq a => Semigroup  (Size [a])
deriving newtype instance Eq a => Monoid     (Size [a])
deriving newtype instance Eq a => PartialOrd (Size [a])

instance Eq a => ExactBounded (Size (ListFraction a)) (Size [a]) where
    toExact (Size n) = Size (toExact n)
    toLowerBound (Size r) = Size (toLowerBound r)
    toUpperBound (Size r) = Size (toUpperBound r)

instance Eq a => Apportion (Size [a]) where
    type Weight (Size [a]) = Size Natural
    apportionMaybe (Size f) ws = do
        chunkSizes <- maybeChunkSizes
        Just $ Size <$> NE.unfoldr makeChunk (chunkSizes, f)
      where
        maybeChunkSizes :: Maybe (NonEmpty Int)
        maybeChunkSizes = fmap (fromIntegral @Natural @Int . getSum)
            <$> apportionMaybe
                (Sum $ fromIntegral @Int @Natural $ L.length f)
                (Sum . getSize <$> ws)

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (l :| mls, r) = case NE.nonEmpty mls of
            Just ls -> (prefix, Just (ls, suffix))
            Nothing -> (r, Nothing)
          where
            (prefix, suffix) = L.splitAt l r

instance Eq a => BoundedApportion (Size [a]) where
    type Exact (Size [a]) = Size (ListFraction a)

--------------------------------------------------------------------------------
-- Instances: ListFraction
--------------------------------------------------------------------------------

deriving newtype instance Eq a => Semigroup  (Size (ListFraction a))
deriving newtype instance Eq a => Monoid     (Size (ListFraction a))

instance Eq a => Apportion (Size (ListFraction a)) where
    type Weight (Size (ListFraction a)) = Size NaturalRatio
    apportionMaybe (Size f) ws = do
        chunkSizes <- maybeChunkSizes
        Just $ Size <$> NE.unfoldr makeChunk (chunkSizes, f)
      where
        maybeChunkSizes :: Maybe (NonEmpty NaturalRatio)
        maybeChunkSizes = fmap getSum
            <$> apportionMaybe
                (Sum $ LF.length f)
                (Sum . getSize <$> ws)

        makeChunk
            :: (NonEmpty NaturalRatio, ListFraction a)
            -> (ListFraction a, Maybe (NonEmpty NaturalRatio, ListFraction a))
        makeChunk (l :| mls, r) = case NE.nonEmpty mls of
            Just ls -> (prefix, Just (ls, suffix))
            Nothing -> (r, Nothing)
          where
            (prefix, suffix) = LF.splitAt l r

instance Eq a => ExactApportion (Size (ListFraction a))


{-
instance Eq a => Apportion (SublistLength [a]) where
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
-}
{-instance Ord a => BoundedApportion (Sublist a) where
    boundedApportionOrder = (<=)
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
