module Main (main) where

import Control.Monad (replicateM)
import Data.Maybe (listToMaybe)
import Data.Sized (Sized)
import Data.Sized qualified as Sized
import Data.Vector (Vector)
import Math.GradientDescent (Config (..), ErrorFunc, Solution (..), gradientDescent)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Tasty (defaultMain)
import Test.Tasty.Falsify (Property, gen, testProperty)

easyConcave :: Sized Vector n Double -> ErrorFunc n
easyConcave ss xs =
  sum $
    Sized.zipWith
      (\x s -> x * x * realToFrac s)
      xs
      ss

easyConcaveProp :: Property ()
easyConcaveProp = do
  let cfg = Config {cfgInitialStepSize = 1, cfgGrow = 2, cfgShrink = 0.5}
  let scales :: Sized Vector 3 Double = Sized.unsafeFromList' [0.1, 0.2, 0.3]
  [x, y, z] <-
    replicateM 3 $
      realToFrac <$> gen (Gen.integral $ Range.withOrigin (-10, 10) (0 :: Int))
  let initial :: Sized Vector 3 Double = Sized.unsafeFromList' [x, y, z]
  let steps = take 10 $ gradientDescent cfg (easyConcave scales) initial
  let solutions = concatMap (either (const []) pure) steps
  case listToMaybe (reverse solutions) of
    Nothing -> fail "No solutions found"
    Just Solution {..}
      | solError < 0.1 -> pure ()
      | otherwise -> fail $ "Best solution was poor: " <> show solError

main :: IO ()
main =
  defaultMain $
    testProperty "Solve easy concave in 10 steps" easyConcaveProp
