module Math.AD
  ( One (..),
    Zero (..),
    Value (..),
    constant,
    variable,
  )
where

data Value a = Value
  { valZero :: a,
    valFirst :: a
  }
  deriving (Show, Eq, Ord)

class One a where one :: a

class Zero a where zero :: a

constant :: Zero a => a -> Value a
constant valZero = Value {valZero, valFirst = zero}

variable :: One a => a -> Value a
variable valZero = Value {valZero, valFirst = one}

instance (Num a, Zero a) => Num (Value a) where
  Value x x' + Value y y' = Value (x + y) (x' + y')
  Value x x' * Value y y' = Value (x * y) (x' * y + x * y')
  abs = error "abs Math.AD.Value"
  signum = error "signum Math.AD.Value"
  fromInteger = constant . fromInteger
  negate = (* fromInteger (-1))

instance (Fractional a, Zero a) => Fractional (Value a) where
  fromRational = constant . fromRational
  recip (Value x x') = Value (recip x) (x' / (x * x))

instance (Floating a, Zero a) => Floating (Value a) where
  pi = constant pi
  exp (Value x x') = Value (exp x) (exp x * x')
  log (Value x x') = Value (log x) (x' / x)
  sin (Value x x') = Value (sin x) (cos x * x')
  cos (Value x x') = Value (cos x) (negate (sin x) * x')
  asin = error "asin Math.AD.Value"
  acos = error "acos Math.AD.Value"
  atan = error "atan Math.AD.Value"
  sinh = error "sinh Math.AD.Value"
  cosh = error "cosh Math.AD.Value"
  asinh = error "asinh Math.AD.Value"
  acosh = error "acosh Math.AD.Value"
  atanh = error "atanh Math.AD.Value"

instance Zero Double where zero = 0

instance One Double where one = 1
