module Math.AD (Value (..), variable, constant) where

import Data.Foldable (fold)

noDerivativeMsg :: String -> String
noDerivativeMsg x = fold ["No derivative for '", x, "'"]

-- | Scalar result along with its first derivative
data Value a = Value !a !a
  deriving (Show, Eq, Ord)

variable, constant :: Num a => a -> Value a
variable x = Value x 1
constant x = Value x 0

instance Num a => Num (Value a) where
  Value l l' + Value r r' = Value (l + r) (l' + r')
  Value l l' * Value r r' = Value (l * r) ((l' * r) + (l * r'))
  abs = error $ noDerivativeMsg "abs"
  signum = error $ noDerivativeMsg "signum"
  fromInteger = constant . fromInteger
  negate = (*) (fromInteger (-1))

instance Fractional a => Fractional (Value a) where
  fromRational = constant . fromRational
  recip (Value x x') = Value (1 / x) (x' / (x * x))

instance Floating a => Floating (Value a) where
  pi = constant pi
  exp (Value x x') = Value (exp x) (exp x * x')
  log (Value x x') = Value (log x) (x' / x)
  sqrt (Value x x') = Value (sqrt x) (x' / (2 * sqrt x))
  sin (Value x x') = Value (sin x) (x' * cos x)
  cos (Value x x') = Value (cos x) (x' * negate (sin x))
  asin = error $ noDerivativeMsg "asin"
  acos = error $ noDerivativeMsg "acos"
  atan = error $ noDerivativeMsg "atan"
  sinh = error $ noDerivativeMsg "sinh"
  cosh = error $ noDerivativeMsg "cosh"
  asinh = error $ noDerivativeMsg "asinh"
  acosh = error $ noDerivativeMsg "acosh"
  atanh = error $ noDerivativeMsg "atanh"
