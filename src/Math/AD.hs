module Math.AD (AD (..), Value (..), variable, constant) where

class AD e a | a -> e where
  intoDouble :: a -> Either e Double
  addScaled :: a -> Double -> a -> Either e a
  one :: a
  zero :: a

-- | Zeroth and first derivative
data Value a = Value
  { valZero :: !a,
    valFirst :: !a
  }
  deriving (Show, Eq, Ord)

variable, constant :: AD e a => a -> Value a
variable x = Value x one
constant x = Value x zero
