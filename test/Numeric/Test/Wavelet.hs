module Numeric.Test.Wavelet (
  tests
  )
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import qualified Numeric.Test.Wavelet.Dyadic as Dyadic
import Numeric.Wavelet

prop_lowPassTest :: Double -> Double -> Bool
prop_lowPassTest x y = lowPass x y == 0.5 * (x + y)

prop_highPassTest :: Double -> Double -> Bool
prop_highPassTest x y = lowPass x y == 0.5 * (y - x)


tests = testGroup "wavlet" [ Dyadic.tests
                           , testProperty "low pass averages" prop_lowPassTest
                           ]
