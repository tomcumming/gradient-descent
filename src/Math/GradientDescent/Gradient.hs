module Math.GradientDescent.Gradient
  ( HasParams (..),
    ScoreFn,
    gradient,
  )
where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (Reader, reader, runReader)
import Control.Monad.State.Strict (evalState, state)
import Math.AD qualified as AD

class Monad m => HasParams v m a where
  param :: v -> m (AD.Value a)

instance AD.Zero a => HasParams a Identity a where
  param = Identity . AD.constant

instance
  (AD.Zero a, AD.One a) =>
  HasParams (Int, a) (Reader Int) a
  where
  param (i, x) = reader $ \di ->
    if i == di
      then AD.variable x
      else AD.constant x

type ScoreFn p a = forall v m. HasParams v m a => p v -> m (AD.Value a)

gradient ::
  (Traversable p, AD.Zero a, AD.One a) =>
  ScoreFn p a ->
  p a ->
  p a
gradient scoreFn params = gradientForVar indexed scoreFn . fst <$> indexed
  where
    indexed =
      evalState
        ( traverse
            (\x -> state $ \idx -> ((idx, x), succ idx))
            params
        )
        (0 :: Int)

gradientForVar ::
  (AD.Zero a, AD.One a) => p (Int, a) -> ScoreFn p a -> Int -> a
gradientForVar indexed scoreFn = AD.valFirst . runReader (scoreFn indexed)
