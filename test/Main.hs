module Main (main) where

import Control.Monad (replicateM)
import Data.Maybe (listToMaybe)
import Data.Sized (Sized)
import Data.Sized qualified as Sized
import Data.Type.Ordinal (ordToNatural)
import Data.Vector (Vector)
import GHC.TypeLits (KnownNat)
import Math.GradientDescent (ErrorFunc, Solution (..), defaultConfig, gradientDescent)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Tasty (defaultMain)
import Test.Tasty.Falsify (Property, gen, testProperty)

easyConcave :: KnownNat n => Sized Vector n Double -> ErrorFunc Fractional n
easyConcave origin =
  sum
    . Sized.zipWithSame
      (\s x -> s * x * x)
      (Sized.generate' $ \idx -> 0.1 * fromIntegral (1 + ordToNatural idx))
    . Sized.zipWithSame
      (\o x -> x - realToFrac o)
      origin

easyConcaveProp :: Property ()
easyConcaveProp = do
  let zero :: Sized Vector 3 Double = Sized.replicate' 0
  [x, y, z] <-
    replicateM 3 $
      realToFrac <$> gen (Gen.integral $ Range.withOrigin (-10, 10) (0 :: Int))
  let origin :: Sized Vector 3 Double = Sized.unsafeFromList' [x, y, z]
  let steps = take 10 $ gradientDescent @Fractional defaultConfig (easyConcave origin) zero
  let solutions = concatMap (either (const []) pure) steps
  case listToMaybe (reverse solutions) of
    Nothing -> fail "No solutions found"
    Just Solution {..}
      | solError < 0.01 -> pure ()
      | otherwise -> fail $ "Best solution was poor: " <> show solError

main :: IO ()
main =
  defaultMain $
    testProperty "Solve easy concave in 10 steps" easyConcaveProp
