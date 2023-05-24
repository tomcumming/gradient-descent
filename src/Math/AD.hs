module Math.AD (Value (..), variable, constant) where

-- | Scalar result along with its first derivative
data Value a = Value !a !a
  deriving (Show, Eq, Ord)

variable, constant :: Num a => a -> Value a
variable x = Value x 1
constant x = Value x 0

instance Num a => Num (Value a) where
  Value l l' + Value r r' = Value (l + r) (l' + r')
  Value l l' * Value r r' = Value (l * r) ((l' * r) + (l * r'))
  abs = error "Value abs"
  signum = error "Value signum"
  fromInteger = constant . fromInteger
  negate = (*) (fromInteger (-1))

instance Fractional a => Fractional (Value a) where
  fromRational = constant . fromRational
  recip (Value x x') = Value (1 / x) (x' / (x * x))
