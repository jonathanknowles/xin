{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Value
    ( Balance
    , Coin
    , HasAssets (..)
    , Assets (..)
    , Values (..)
    , coinToBalance
    , balanceToCoins
    ) where

import Algebra.Apportion.Balanced
    ( BalancedApportion (..) )
import AsList
    ( AsList (..), asList )
import Data.Coerce
    ( coerce )
import Data.Group
    ( Group (..) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Maybe
    ( fromMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Strict.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Algebra.Apportion.Balanced as BalancedApportion
import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- SumMap
--------------------------------------------------------------------------------

newtype SumMap a i = SumMap
    {unSumMap :: MonoidMap a (Sum i)}
    deriving (Arbitrary, Read, Show) via (AsList (SumMap a i))

instance (Ord a, Eq i, Num i) => IsList (SumMap a i) where
    type Item (SumMap a i) = (a, i)
    fromList = SumMap . fromList . fmap (fmap Sum)
    toList = fmap (fmap getSum) . toList . unSumMap

--------------------------------------------------------------------------------
-- AssetValueMap
--------------------------------------------------------------------------------

newtype AssetValueMap a i = AssetValueMap
    {unAssetValueMap :: MonoidMap a (Sum i)}

class HasAssets a where
    type Asset a
    type Value a
    filterAssets :: (Asset a -> Bool) -> a -> a
    getAssets :: a -> Set (Asset a)
    getAssetValue :: Ord (Asset a) => Asset a -> a -> Value a
    setAssetValue :: Ord (Asset a) => Asset a -> Value a -> a -> a
    singleton :: Asset a -> Value a -> a

instance (Ord a, Eq i, Num i) => HasAssets (AssetValueMap a i) where
    type Asset (AssetValueMap a i) = a
    type Value (AssetValueMap a i) = i
    filterAssets f =
        AssetValueMap . fromList . filter (f . fst) . toList . unAssetValueMap
    getAssets =
        MonoidMap.nonNullKeys . unAssetValueMap
    getAssetValue a =
        getSum . MonoidMap.get a . unAssetValueMap
    setAssetValue a =
        coerce . MonoidMap.set a . Sum
    singleton a =
        AssetValueMap . MonoidMap.singleton a . Sum

--------------------------------------------------------------------------------
-- Balance
--------------------------------------------------------------------------------

newtype Balance a = Balance (MonoidMap a (Sum Integer))
    deriving (Arbitrary, IsList, Read, Show) via SumMap a Integer
    deriving HasAssets via AssetValueMap a Integer
    deriving newtype (Eq, Semigroup, Commutative, Monoid, MonoidNull, Group)

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

newtype Coin a = Coin (MonoidMap a (Sum Natural))
    deriving (Arbitrary, IsList, Read, Show) via SumMap a Natural
    deriving HasAssets via AssetValueMap a Natural
    deriving newtype (Eq, Semigroup, Commutative, Monoid, MonoidNull)
    deriving newtype (LeftReductive, RightReductive, Reductive)
    deriving newtype (LeftCancellative, RightCancellative, Cancellative)
    deriving newtype (OverlappingGCDMonoid, Monus, PositiveMonoid)

newtype Assets a = Assets {unAssets :: a}
    deriving Show

newtype Values a = Values {unValues :: a}
    deriving Show

deriving via BalancedApportion.Keys
    (MonoidMap a (Sum Natural))
    instance Ord a =>
    BalancedApportion (Assets (Coin a))

deriving via BalancedApportion.Values
    (MonoidMap a (Sum Natural))
    instance Ord a =>
    BalancedApportion (Values (Coin a))

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

coinToBalance :: Ord a => Coin a -> Balance a
coinToBalance = asList $ fmap $ fmap intCast

balanceToCoins :: forall a. Ord a => Balance a -> (Coin a, Coin a)
balanceToCoins b = (balanceToCoin (invert b), balanceToCoin b)
  where
    balanceToCoin :: Balance a -> Coin a
    balanceToCoin = asList $ fmap $ fmap $ fromMaybe 0 . intCastMaybe
