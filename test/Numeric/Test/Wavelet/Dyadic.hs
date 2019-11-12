{-# LANGUAGE TemplateHaskell #-}
module Numeric.Test.Wavelet.Dyadic where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Numeric.Wavelet.Dyadic

prop_testDyadicRange :: Positive Integer -> Bool
prop_testDyadicRange n =
  let m = getPositive n
      j = dyadicRange m
  in case j of
    0 -> m <= 1
    1 -> m <= 2
    j -> m <= 2^j && m > 2^(j-1)

data Subinterval = Subinterval (Integer, Integer) (Integer, Integer) deriving Show

instance Arbitrary Subinterval where
  arbitrary = do 
    a <- arbitrary
    c <- arbitrary `suchThat` \x -> x >= a
    d <- arbitrary `suchThat` \x -> x >= c
    b <- arbitrary `suchThat` \x -> x >= d
    return $ Subinterval (a, b) (c, d)


-- Check if the range vector is correctly generated
prop_testRangeVactor :: Subinterval -> Bool
prop_testRangeVactor (Subinterval (a, b) (c, d)) =
  let r = rangeVector (a, b) (c, d)
      rz = zip r [a..b]
      rzcd = (\(x,_) -> x == 1) <$> filter (\(_,i) -> i >= c && i <= d) rz
      rzab = (\(x,_) -> x == 0) <$> filter (\(_,i) -> not (i >= c && i <= d)) rz
  in fromIntegral (length r) == b - a + 1 && and rzcd && and rzab


data DyadicInterval = DyadicInterval Integer Integer Integer Integer deriving Show

instance Arbitrary DyadicInterval where
  arbitrary = do
    n <- getPositive <$> arbitrary :: Gen Integer
    let logN = dyadicRange n
    j <- choose (1, logN)
    k <- choose (0, 2^j -1)
    return $ DyadicInterval n logN j k

prop_testDyadicVector :: DyadicInterval -> Bool
prop_testDyadicVector (DyadicInterval n logN j k) =
  let r = dyadicVector j k logN n
  in if n == 1 || n == 0
  then length r == 0
  else fromIntegral (length r) == n


prop_testDyadicSequence :: Positive Integer -> Bool
prop_testDyadicSequence x =
  let logN = dyadicRange $ getPositive x
      s = dyadicSequence logN
      extractLevel i (j, _, _) = j == i
      ss = (\i -> filter (extractLevel i) s) <$> [0..(logN-1)]
  in length s == (2^logN - 1)

prop_testWaveletCoeff :: Positive Integer -> Bool
prop_testWaveletCoeff x =
  let logN = dyadicRange $ getPositive x
      n = 2^logN
      coeffs = (\(j, k, _) -> waveletCoefficient j k logN (fromIntegral n)) <$> dyadicSequence logN
      seq = [0..(n-2)]
  in if not (null coeffs)
        then length coeffs == length seq && 
             and [ abs (0 - dotProduct (coeffs !! (fromIntegral i) ) (coeffs !! (fromIntegral j))) < 1e-10 | i <- seq, j <- seq, i /= j ]
        else True

prop_testWaveletCoeffUnit :: Positive Integer -> Bool
prop_testWaveletCoeffUnit x =
  let logN = dyadicRange $ getPositive x
      n = 2^logN
      coeffs = (\(j, k, _) -> waveletCoefficient j k logN (fromIntegral n)) <$> dyadicSequence logN
  in if not (null coeffs)
        then and [ abs (1.0 - dotProduct c c) < 1e-10 | c <- coeffs ]
        else True

data WaveletData = WaveletData { _logN :: Integer, _data :: [ Double ] } deriving Show

instance Arbitrary WaveletData where

  arbitrary = do
    m <- (getPositive <$> arbitrary) `suchThat` \x -> x > 2
    let logN = dyadicRange m
        n = 2^logN
    d <- vector n
    return $ WaveletData { _logN = logN, _data = d }


prop_testWaveletTransform :: WaveletData -> Bool
prop_testWaveletTransform w =
  let logN = _logN w
      n = fromIntegral $ 2^logN
      coeffs = (\(j, k, _) -> waveletCoefficient j k logN n) <$> dyadicSequence logN
      s = scaleFactor 0 logN
      ave = (* s) <$> averageCoefficient logN (fromIntegral n)
      wcoeffs = ave : coeffs
      d = _data w
      ws = zipWith dotProduct wcoeffs (repeat d)
  in abs (dotProduct d d - dotProduct ws ws) < 1e-6


tests = testGroup "dyadic" [ testProperty "dyadic range is calculated correctly" prop_testDyadicRange
                           , testProperty "range vector maps subset properly" prop_testRangeVactor
                           , testProperty "dyadic vector is correctly generated for given size" prop_testDyadicVector
                           , testProperty "A number N is correctly mapped to a dyadic sequence" prop_testDyadicSequence
                           , testProperty "wavelet coefficients are orthogonal" prop_testWaveletCoeff
                           , testProperty "wavelet coefficients are unit vectors" prop_testWaveletCoeffUnit
                           , testProperty "energy of wavelet tranform is same as data" prop_testWaveletTransform
                           ]

