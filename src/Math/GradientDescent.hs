module Math.GradientDescent
  ( Config (..),
    Solution (..),
    Step (..),
    IntoScalar (..),
    Correct (..),
    Error (..),
    defaultConfig,
    gradientDescent,
    score,
  )
where

import Control.Monad.Identity (Identity (..), runIdentity)
import Control.Monad.State.Strict (evalState, state)
import Data.Foldable (toList)
import Math.AD qualified as AD
import Math.GradientDescent.Gradient

class (Num s, Ord s) => IntoScalar s a | a -> s where
  intoScalar :: a -> Maybe s

-- | \k x' x -> x - k * x'
class Correct s a | a -> s where
  correct :: s -> a -> a -> a

data Config p s a = Config
  { cfgInitialStepSize :: s,
    cfgGrow :: s,
    cfgShrink :: s,
    -- | Normalize parameters after moving down gradient
    cfgNormalize :: p a -> p a
  }

data Solution p s a = Solution
  { solError :: s,
    solParams :: p a
  }
  deriving (Show)

defaultConfig :: Fractional s => Config p s a
defaultConfig =
  Config
    { cfgInitialStepSize = 1,
      cfgGrow = 2,
      cfgShrink = 0.5,
      cfgNormalize = id
    }

data Step p s a
  = -- | New guess was no better, new step size
    Miss s
  | Improvement (Solution p s a)
  deriving (Show)

newtype Error a
  = ScoreNonScalar a
  deriving (Show)

gradientDescent ::
  forall s a p.
  (Traversable p, IntoScalar s a, Correct s a, AD.Zero a, AD.One a) =>
  Config p s a ->
  ScoreFn p a ->
  p a ->
  [Either (Error a) (Step p s a)]
gradientDescent Config {..} scoreFn initial = case score scoreFn initial of
  Left e -> [Left e]
  Right s -> findSolution cfgInitialStepSize initial s
  where
    findSolution :: s -> p a -> s -> [Either (Error a) (Step p s a)]
    findSolution stepSize ps s =
      findStep True stepSize ps s $ gradient scoreFn ps

    findStep :: Bool -> s -> p a -> s -> p a -> [Either (Error a) (Step p s a)]
    findStep firstTry stepSize ps s grad =
      let next = do
            let nextPs = cfgNormalize $ correctParams stepSize grad ps
            nextScore <- score scoreFn nextPs
            pure (nextPs, nextScore)
       in case next of
            Left e -> [Left e]
            Right (nextPs, nextScore) ->
              if nextScore < s
                then
                  Right (Improvement (Solution nextScore nextPs))
                    : findSolution
                      (stepSize * if firstTry then cfgGrow else 1)
                      nextPs
                      nextScore
                else
                  Right (Miss stepSize)
                    : findStep False (cfgShrink * stepSize) ps s grad

-- | scale -> grad -> params -> new params
correctParams :: (Traversable p, Correct s a) => s -> p a -> p a -> p a
correctParams s xs' xs =
  evalState
    ( traverse
        ( \x -> state $ \case
            [] -> error "Gradient and parameters of different length"
            (x' : xs'') -> (correct s x' x, xs'')
        )
        xs
    )
    (toList xs')

score ::
  (Functor p, IntoScalar s a, AD.Zero a) =>
  ScoreFn p a ->
  p a ->
  Either (Error a) s
score f ps = maybe (Left $ ScoreNonScalar val) Right $ intoScalar val
  where
    val = AD.valZero $ runIdentity $ f ps

instance IntoScalar Double Double where intoScalar = Just

instance Correct Double Double where correct k x' x = x - k * x'
