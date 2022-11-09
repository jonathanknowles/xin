{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChangeSpec where

import Test.Hspec
    ( Spec )
import Test.QuickCheck.Instances.NonEmpty
    ()

spec :: Spec
spec = pure ()
{-
    parallel $ describe "makeChangeForAsset" $ do
        it "prop_makeChangeForAsset_length" $
            prop_makeChangeForAsset_length
                & property
        it "prop_makeChangeForAsset_order" $
            prop_makeChangeForAsset_order
                & property
        it "prop_makeChangeForAsset_remainder" $
            prop_makeChangeForAsset_remainder
                & property
        it "prop_makeChangeForAsset_sum" $
            prop_makeChangeForAsset_sum
                & property

    parallel $ describe "makeChangeForCoin" $ do
        it "prop_makeChangeForCoin_length" $
            prop_makeChangeForCoin_length
                & property
        it "prop_makeChangeForCoin_order" $
            prop_makeChangeForCoin_order
                & property
        it "prop_makeChangeForCoin_remainder" $
            prop_makeChangeForCoin_remainder
                & property
        it "prop_makeChangeForCoin_sum" $
            prop_makeChangeForCoin_sum
                & property

prop_makeChangeForAsset_length
    :: CoinValue
    -> NonEmpty (TestPriority, CoinValue)
    -> Property
prop_makeChangeForAsset_length n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    NE.length (snd (makeChangeForAsset (n, pws))) === NE.length pws
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForAsset_order
    :: CoinValue
    -> NonEmpty (TestPriority, CoinValue)
    -> Property
prop_makeChangeForAsset_order n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    fmap fst (snd (makeChangeForAsset (n, pws)))
        === fmap fst pws
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForAsset_remainder
    :: CoinValue
    -> NonEmpty (TestPriority, CoinValue)
    -> Property
prop_makeChangeForAsset_remainder n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    fst (makeChangeForAsset (n, pws))
        === if weightSum == 0 then n else 0
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForAsset_sum
    :: CoinValue
    -> NonEmpty (TestPriority, CoinValue)
    -> Property
prop_makeChangeForAsset_sum n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    F.fold (fmap snd (snd (makeChangeForAsset (n, pws))))
        === if weightSum == 0 then 0 else n
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_length
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_length n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    NE.length (snd (makeChangeForCoin (n, pws))) === NE.length pws
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_order
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_order n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    fmap fst (snd (makeChangeForCoin (n, pws)))
        === fmap fst pws
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_remainder
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_remainder n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    fst (makeChangeForCoin (n, pws))
        === filterAssets (`Set.notMember` weightAssets) n
  where
    weightAssets = F.foldMap getAssets (snd <$> pws)
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_sum
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_sum n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    foldMap snd (snd (makeChangeForCoin (n, pws)))
        === filterAssets (`Set.member` commonAssets) n
  where
    commonAssets = weightAssets `Set.intersection` targetAssets
    targetAssets = getAssets n
    weightAssets = F.foldMap getAssets (snd <$> pws)
    weightSum = F.foldMap snd pws

type TestCoin = Coin TestAsset

newtype TestAsset = TestAsset (Latin Quid)
    deriving stock (Eq, Ord, Read, Show)
    deriving Arbitrary via (Size 4 Quid)

data TestPriority
    = TestPriority0
    | TestPriority1
    | TestPriority2
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Arbitrary TestPriority where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum
-}
