module SinWave
  ( scoreFn,
    genParams,
  )
where

import Control.Monad (replicateM)
import Math.AD qualified as AD
import Math.GradientDescent.Gradient (ScoreFn, param)
import Test.Falsify.Generator (integral)
import Test.Falsify.Range (withOrigin)
import Test.Tasty.Falsify (Gen)

truth :: [Double]
truth = [0.75, 1.0, 1.25]

testPoints :: [Double]
testPoints = (/ (10 * pi)) <$> [0 .. 10]

scoreFn :: ScoreFn [] Double
scoreFn prms = do
  prms_ <- traverse param prms
  let truth_ = AD.constant <$> truth
  let errors = do
        x <- AD.constant <$> testPoints
        (t, p) <- zip truth_ prms_
        pure $ sin (x * t) - sin (x * p)
  let totalError = sum errors
  pure $ totalError * totalError

genParams :: Gen [Double]
genParams =
  replicateM (length truth) $
    (/ 100) . fromIntegral <$> (integral @Int $ withOrigin (50, 150) 100)
