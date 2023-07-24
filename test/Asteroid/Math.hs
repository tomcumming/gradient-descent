module Asteroid.Math where

import Math.AD qualified as AD
import Math.GradientDescent (Correct (..), IntoScalar (..))

data MV
  = Complex
  | Scalar Double
  | V2 Double Double
  deriving (Show, Eq)

isZero :: MV -> Bool
isZero = \case
  Scalar 0 -> True
  V2 0 0 -> True
  _ -> False

instance IntoScalar Double MV where
  intoScalar = \case
    Scalar s -> Just s
    _ -> Nothing

instance Correct Double MV where
  correct k x' x = add' x (inner' (Scalar (-k)) x')

instance AD.One MV where
  one = Scalar 1

instance AD.Zero MV where
  zero = Scalar 0

add' :: MV -> MV -> MV
add' l r = case (l, r) of
  _ | isZero l -> r
  _ | isZero r -> l
  (Scalar ls, Scalar rs) -> Scalar $ ls + rs
  (V2 lx ly, V2 rx ry) -> V2 (lx + rx) (ly + ry)
  _ -> Complex

inner' :: MV -> MV -> MV
inner' l r = case (l, r) of
  _ | isZero l || isZero r -> Scalar 0
  (Complex, _) -> Complex
  (_, Complex) -> Complex
  (Scalar ls, Scalar rs) -> Scalar $ ls * rs
  (Scalar ls, V2 rx ry) -> V2 (ls * rx) (ls * ry)
  (V2 lx ly, Scalar rs) -> V2 (lx * rs) (ly * rs)
  (V2 lx ly, V2 rx ry) -> Scalar $ lx * rx + ly * ry

add :: AD.Value MV -> AD.Value MV -> AD.Value MV
add (AD.Value x x') (AD.Value y y') =
  AD.Value (add' x y) (add' x' y')

inner :: AD.Value MV -> AD.Value MV -> AD.Value MV
inner (AD.Value x x') (AD.Value y y') =
  AD.Value
    (inner' x y)
    ( add'
        (inner' x' y)
        (inner' x y')
    )

sub :: AD.Value MV -> AD.Value MV -> AD.Value MV
sub l r = add l $ inner (AD.constant $ Scalar (-1)) r
