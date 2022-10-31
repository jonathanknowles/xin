{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.NewApportion
    where

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
import Data.These
    ( These (..) )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( last, zip, zipWith )

import qualified Data.List.NonEmpty.Extended as NE

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

class (Eq a, Monoid a, Monoid (Weight a)) => Apportion a where

    type Weight a

    apportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default apportion
        :: a -> NonEmpty (Weight a) -> Partition a
    apportion a as = case apportionMaybe a as of
        Nothing -> Partition a (mempty <$ as)
        Just bs -> Partition mempty bs

    apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       Partition b bs | b == mempty -> Just bs
       _ -> Nothing

apportionLaw_length :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_length a ws =
    length (partition (apportion a ws)) == length ws

apportionLaw_maybe :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_maybe a ws =
    isJust (apportionMaybe a ws) == (fold1 (partition (apportion a ws)) == a)

apportionLaw_sum :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_sum a ws =
    fold1 (apportion a ws) == a

--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------

class (Apportion a, ExactBalancedApportion (Exact a)) => BalancedApportion a
  where
    type Exact a

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

balancedApportionLaw_identity
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity a ws =
    balancedApportion a ws == apportion a ws

balancedApportionLaw_identity_maybe
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity_maybe a ws =
    balancedApportionMaybe a ws == apportionMaybe a ws

--------------------------------------------------------------------------------
-- ExactBalancedApportion
--------------------------------------------------------------------------------

class BalancedApportion a => ExactBalancedApportion a where

    exactBalancedApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default exactBalancedApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    exactBalancedApportion = apportion

    exactBalancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default exactBalancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    exactBalancedApportionMaybe = apportionMaybe

exactBalancedApportionLaw_identity
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_identity a ws =
    exactBalancedApportion a ws == apportion a ws

exactBalancedApportionLaw_identity_maybe
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_identity_maybe a ws =
    exactBalancedApportionMaybe a ws == apportionMaybe a ws

exactBalancedApportionLaw_folds
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_folds a ws =
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

instance BalancedApportion (Sum (Ratio Natural)) where

    type Exact (Sum (Ratio Natural)) = Sum (Ratio Natural)

instance ExactBalancedApportion (Sum (Ratio Natural))

--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------

instance Apportion (Sum Natural) where

    type Weight (Sum Natural) = Sum Natural

    apportion = undefined
    apportionMaybe = undefined

instance BalancedApportion (Sum Natural) where

    type Exact (Sum Natural) = Sum (Ratio Natural)

--------------------------------------------------------------------------------
-- Roundable
--------------------------------------------------------------------------------

class Roundable a b where
    roundD :: a -> b
    roundU :: a -> b

instance Roundable (Ratio Natural) Natural where
    roundD = floor
    roundU = ceiling

instance Roundable (Sum (Ratio Natural)) (Sum Natural) where
    roundD = fmap floor
    roundU = fmap ceiling

{-
--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------
{-
class
    ( Apportion a
    , Roundable (Fraction a) a
    , Semigroup (Fraction a)
    ) =>
    BalancedApportion a
  where
    type Fraction a
    apportionExact :: a -> NonEmpty (Weight a) -> Partition (Fraction a)
    apportionOrder :: a -> a -> Bool

balancedApportionLaw_length
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_length a ws =
    length (partition (apportionExact a ws)) == length ws

balancedApportionLaw_order_roundD
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_order_roundD a ws =
    zipAll apportionOrder (roundD <$> apportionExact a ws) (apportion a ws)

balancedApportionLaw_order_roundU
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_order_roundU a ws =
    zipAll apportionOrder (apportion a ws) (roundU <$> apportionExact a ws)

balancedApportionLaw_sum_roundD
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_sum_roundD a ws =
    roundD (fold1 (apportionExact a ws)) == a

balancedApportionLaw_sum_roundU
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_sum_roundU a ws =
    roundU (fold1 (apportionExact a ws)) == a
-}
--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------
{-
instance Apportion (Sum Natural) where
    type Weight (Sum Natural) = Natural
    apportionMaybe (Sum n) = fmap (fmap Sum) . Natural.apportion n-}
{-
instance BalancedApportion (Sum Natural) where
    type Fraction (Sum Natural) = Sum (Ratio Natural)
    apportionExact (Sum n) ws
        | total == 0 = Partition (Sum (n % 1)) (Sum (0 % 1)     <$  ws)
        | otherwise  = Partition (Sum (0 % 1)) (Sum . (% total) <$> ws)
      where
        total = sum ws
    apportionOrder = (<=)
-}
--------------------------------------------------------------------------------
-- Instances: List
--------------------------------------------------------------------------------
{-
instance Eq a => Apportion [a] where
    type Weight [a] = Int
    apportionMaybe as ws = do
        chunkLengths <- maybeChunkLengths
        Just $ NE.unfoldr makeChunk (chunkLengths, as)
      where
        maybeChunkLengths :: Maybe (NonEmpty Int)
        maybeChunkLengths =
            fmap (fromIntegral @Natural @Int . getSum) <$>
            apportionMaybe
                (Sum $ fromIntegral $ length as)
                (fromIntegral <$> ws)

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (c :| mcs, bs) = case NE.nonEmpty mcs of
            Just cs -> (prefix, Just (cs, suffix))
            Nothing -> (bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

instance Eq a => Apportion [a] where
    type Weight [a] = Natural
    apportion = undefined
    apportionMaybe = undefined

instance Eq a => BalancedApportion [a] where
    type Fraction [a] = Ratio Natural
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Instances: Map
--------------------------------------------------------------------------------
{-
instance (Ord k, Eq v) => Apportion (Map k v) where
    type Weight (Map k v) = Map k v
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v, Roundable (Fraction v)) => BalancedApportion (Map k v) where
    type Fraction (Map k v) = Fraction v
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Instances: Map Keys
--------------------------------------------------------------------------------
{-
newtype Keys a = Keys
    { unKeys :: a }
    deriving newtype (Eq, Monoid, Semigroup, Show)

instance (Ord k, Eq v) => Apportion (Keys (Map k v)) where
    type Weight (Keys (Map k v)) = Natural
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v) => BalancedApportion (Keys (Map k v)) where
    type Fraction (Keys (Map k v)) = Ratio Natural
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Instances: Map Values
--------------------------------------------------------------------------------
{-
newtype Values a = Values
    { unValues :: a }
    deriving newtype (Eq, Monoid, Semigroup, Show)

instance (Ord k, Eq v, Apportion v) =>
    Apportion (Values (Map k v))
  where
    type Weight (Values (Map k v)) = Weight v
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v, BalancedApportion v) =>
    BalancedApportion (Values (Map k v))
  where
    type Fraction (Values (Map k v)) = Fraction v
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Apportioning with equal weights
--------------------------------------------------------------------------------
{-
apportionEqual
    :: (BalancedApportion a, Integral (Weight a))
    => a
    -> NonEmpty void
    -> NonEmpty a
apportionEqual _a _ws = undefined -- snd (apportion a (1 <$ ws))

apportionEqualN
    :: (BalancedApportion a, Integral (Weight a))
    => a
    -> Int
    -> NonEmpty a
apportionEqualN a n =
    apportionEqual a (() :| replicate (max 0 (n - 1)) ())

bipartition :: (BalancedApportion a, Integral (Weight a)) => a -> (a, a)
bipartition = (NE.head &&& NE.last) . flip apportionEqual (() :| [()])
-}
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

distanceRatioNatural :: Ratio Natural -> Ratio Natural -> Ratio Natural
distanceRatioNatural = undefined

naturalToRatio :: Natural -> Ratio Natural
naturalToRatio = fromIntegral

naturalLength :: Foldable f => f a -> Natural
naturalLength = fromIntegral @Int @Natural . length
{-
apportionEqualMapKeys
    :: (Ord k, Eq v) => Map k v -> NonEmpty void -> NonEmpty (Map k v)
apportionEqualMapKeys m ws = unKeys <$> apportionEqual (Keys m) ws

apportionEqualMapValues
    :: (Ord k, Eq v, BalancedApportion v, Integral (Weight v))
    => Map k v -> NonEmpty void -> NonEmpty (Map k v)
apportionEqualMapValues m ws = unValues <$> apportionEqual (Values m) ws
-}
{-apportionEqualNatural
    :: Sum Natural -> NonEmpty void -> NonEmpty (Sum Natural)
apportionEqualNatural = apportionEqual-}
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
