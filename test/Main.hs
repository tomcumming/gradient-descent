module Main (main) where

import Data.Map qualified as M
import Math.AD qualified as AD
import Math.GradientDescent (ScoreFn, Solution (..), Step (..), defaultConfig, gradientDescent)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate (lt, (.$))
import Test.Falsify.Property (assert, gen)
import Test.Falsify.Range qualified as Range
import Test.Tasty (defaultMain)
import Test.Tasty.Falsify (Property, testProperty)

data Blade
  = Scalar Double
  | V2 Double Double
  deriving (Show)

data MV
  = Complex
  | Zero
  | Blade Blade
  deriving (Show)

scalar :: Double -> MV
scalar = Blade . Scalar

v2 :: Double -> Double -> MV
v2 x y = Blade $ V2 x y

instance AD.AD String MV where
  intoDouble = \case
    Blade (Scalar s) -> pure s
    Zero -> pure 0
    mv -> Left $ "Can't intoDouble: " <> show mv
  addScaled a k b = pure $ add' a (inner' (scalar k) b)
  one = scalar 1
  zero = Zero

add' :: MV -> MV -> MV
add' l r = case (l, r) of
  (Zero, _) -> r
  (_, Zero) -> l
  (Blade (Scalar ls), Blade (Scalar rs)) -> scalar $ ls + rs
  (Blade (V2 lx ly), Blade (V2 rx ry)) -> v2 (lx + rx) (ly + ry)
  _ -> Complex

inner' :: MV -> MV -> MV
inner' l r = case (l, r) of
  (Zero, _) -> Zero
  (_, Zero) -> Zero
  (Complex, _) -> Complex
  (_, Complex) -> Complex
  (Blade lb, Blade rb) -> case (lb, rb) of
    (Scalar ls, Scalar rs) -> scalar $ ls * rs
    (Scalar ls, V2 rx ry) -> v2 (ls * rx) (ls * ry)
    (V2 lx ly, Scalar rs) -> v2 (lx * rs) (ly * rs)
    (V2 lx ly, V2 rx ry) -> scalar $ lx * rx + ly * ry

add :: AD.Value MV -> AD.Value MV -> AD.Value MV
add (AD.Value x x') (AD.Value y y') =
  AD.Value (add' x y) (add' x' y')

inner :: AD.Value MV -> AD.Value MV -> AD.Value MV
inner (AD.Value x x') (AD.Value y y') =
  AD.Value
    (inner' x y)
    (add' (inner' x' y) (inner' x y'))

sub :: AD.Value MV -> AD.Value MV -> AD.Value MV
sub l r = add l $ inner (AD.constant $ scalar (-1)) r

data Param = K | V deriving (Show, Eq, Ord)

-- | This is a toy function to optimise with different shape parameters:
--   k : Scalar
--   v : Two dimensional vector
scoreFn :: ScoreFn Param MV
scoreFn ps
  | Just k <- M.lookup K ps,
    Just v <- M.lookup V ps =
      let target = AD.constant $ v2 2 0
          a = sqrt 0.5
          u = inner k (AD.constant $ v2 a a)
          vPlusU = add v u
          -- v + u = target
          offset = sub target vPlusU
          -- Constrain v and u to same length
          diff = sub (inner v v) (inner u u)
       in add (inner offset offset) (inner diff diff)
  | otherwise = AD.constant Complex

firstSolution :: [Either String (Step Param MV)] -> Either String (Solution Param MV)
firstSolution = \case
  [] -> Left "No solution found"
  Left e : _ -> Left e
  Right (Miss _) : rest -> firstSolution rest
  Right (Improvement s) : _ -> Right s

toyExampleProp :: Property ()
toyExampleProp = do
  let intRange = 1000
  k <-
    gen $
      (/ realToFrac intRange) . realToFrac
        <$> Gen.integral (Range.withOrigin @Int (-intRange, intRange) 0)
  let ps = M.fromList [(K, scalar k), (V, v2 0 0)]
  let initialError = AD.intoDouble $ AD.valZero $ scoreFn $ AD.constant <$> ps
  case initialError of
    Left e -> fail e
    Right ie | ie >= 1.0 -> pure ()
    Right _ -> fail "Initial error should be greater than 1.0"
  let steps = take 20 $ gradientDescent defaultConfig scoreFn ps
  Solution {..} <-
    either
      (fail . show)
      pure
      $ firstSolution
      $ reverse steps
  assert $ lt .$ ("Solution Error", solError) .$ ("Target", 0.1)

main :: IO ()
main =
  defaultMain $
    testProperty "Solve toy example in 20 steps" toyExampleProp
