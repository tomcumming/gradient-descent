module Main (main) where

import Control.Monad (replicateM)
import Data.Maybe (listToMaybe)
import Math.GradientDescent (ErrorFunc, Parameterized, Solution (..), defaultConfig, gradientDescent, parameters)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Tasty (defaultMain)
import Test.Tasty.Falsify (Property, gen, testProperty)

data Params a = Params
  { pX :: a,
    pY :: a,
    pZ :: a
  }
  deriving (Functor, Foldable, Traversable)

instance Parameterized Params where
  parameters _ =
    [ \ps pX -> ps {pX},
      \ps pY -> ps {pY},
      \ps pZ -> ps {pZ}
    ]

easyConcave :: (Double, Double, Double) -> ErrorFunc Fractional Params
easyConcave (ox, oy, oz) Params {..} =
  sum $
    zipWith3
      (\o s p -> s * (p - o) * (p - o))
      (realToFrac <$> [ox, oy, oz])
      [0.1, 0.2, 0.3]
      [pX, pY, pZ]

easyConcaveProp :: Property ()
easyConcaveProp = do
  let zero = Params @Double 0 0 0
  [ox, oy, oz] <-
    replicateM 3 $
      realToFrac <$> gen (Gen.integral $ Range.withOrigin (-10, 10) (0 :: Int))
  let origin = (ox, oy, oz)
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
