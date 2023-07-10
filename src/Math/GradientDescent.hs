module Math.GradientDescent
  ( Config (..),
    Solution (..),
    Step (..),
    ScoreFn,
    defaultConfig,
    gradientDescent,
    score,
    gradient,
  )
where

import Data.Map qualified as M
import Math.AD qualified as AD

data Config k a = Config
  { cfgInitialStepSize :: Double,
    cfgGrow :: Double,
    cfgShrink :: Double,
    -- | Normalize parameters after moving down gradient
    cfgNormalize :: M.Map k a -> M.Map k a
  }

type ScoreFn k a = M.Map k (AD.Value a) -> AD.Value a

data Solution k a = Solution
  { solError :: Double,
    solParams :: M.Map k a
  }
  deriving (Show)

defaultConfig :: Config t a
defaultConfig =
  Config
    { cfgInitialStepSize = 1,
      cfgGrow = 2,
      cfgShrink = 0.5,
      cfgNormalize = id
    }

data Step k a
  = -- | New guess was no better, new step size
    Miss Double
  | Improvement (Solution k a)
  deriving (Show)

gradientDescent ::
  forall e k a.
  (Ord k, AD.AD e a, Show k, Show a) =>
  Config k a ->
  ScoreFn k a ->
  M.Map k a ->
  [Either e (Step k a)]
gradientDescent Config {..} scoreFn initial = case score scoreFn initial of
  Left e -> [Left e]
  Right s -> findSolution cfgInitialStepSize initial s
  where
    findSolution :: Double -> M.Map k a -> Double -> [Either e (Step k a)]
    findSolution stepSize ps s = findStep True stepSize ps s $ gradient scoreFn ps

    findStep :: Bool -> Double -> M.Map k a -> Double -> M.Map k a -> [Either e (Step k a)]
    findStep firstTry stepSize ps s grad =
      let next = do
            nextPs <- cfgNormalize <$> subScaledGradient ps stepSize grad
            nextScore <- score scoreFn nextPs

            pure (nextPs, nextScore)
       in case next of
            Left e -> [Left e]
            Right (nextPs, nextScore) ->
              if nextScore < s
                then
                  Right (Improvement (Solution nextScore nextPs))
                    : findSolution (stepSize * if firstTry then cfgGrow else 1) nextPs nextScore
                else
                  Right (Miss stepSize)
                    : findStep False (cfgShrink * stepSize) ps s grad

subScaledGradient ::
  (AD.AD e a, Ord k) =>
  M.Map k a ->
  Double ->
  M.Map k a ->
  Either e (M.Map k a)
subScaledGradient ps k =
  sequence
    . M.intersectionWith
      (\p g -> AD.addScaled p (-k) g)
      ps

score :: AD.AD e a => ScoreFn k a -> M.Map k a -> Either e Double
score f ps = AD.intoDouble . AD.valZero $ f (AD.constant <$> ps)

gradient ::
  forall e k a.
  (Ord k, AD.AD e a) =>
  ScoreFn k a ->
  M.Map k a ->
  M.Map k a
gradient f ps = AD.valFirst <$> M.mapWithKey go ps
  where
    ps' = AD.constant <$> ps
    go k p = f $ M.insert k (AD.variable p) ps'
