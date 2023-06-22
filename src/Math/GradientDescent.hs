module Math.GradientDescent
  ( Parameterized (..),
    ErrorFunc,
    Config (..),
    Solution (..),
    defaultConfig,
    gradientDescent,
    gradient,
  )
where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL)
import Math.AD qualified as AD

class Traversable t => Parameterized t where
  parameters :: t a -> [t a -> a -> t a]

type ErrorFunc c f = forall a. c a => f a -> a

data Config t a = Config
  { cfgInitialStepSize :: a,
    cfgGrow :: a,
    cfgShrink :: a,
    -- | Normalize parameters after moving down gradient
    cfgNormalize :: t a -> t a
  }

data Solution t a = Solution
  { solError :: a,
    solParams :: t a
  }
  deriving (Show)

defaultConfig :: Fractional a => Config t a
defaultConfig =
  Config
    { cfgInitialStepSize = 1,
      cfgGrow = 2,
      cfgShrink = 0.5,
      cfgNormalize = id
    }

gradientDescent ::
  forall c t a.
  (c a, c (AD.Value a), Parameterized t, Ord a, Num a) =>
  Config t a ->
  ErrorFunc c t ->
  t a ->
  [Either a (Solution t a)]
gradientDescent Config {..} errFn initial =
  let initialErr = errFn initial
   in Right (Solution initialErr initial) : findSolution cfgInitialStepSize initial (errFn initial)
  where
    findSolution :: a -> t a -> a -> [Either a (Solution t a)]
    findSolution stepSize xs err = findStep True stepSize xs err (gradient @c errFn xs)

    findStep :: Bool -> a -> t a -> a -> t a -> [Either a (Solution t a)]
    findStep firstTry stepSize xs err xs' =
      let nextXs = cfgNormalize $ zipWithT (\x x' -> x - x' * stepSize) xs xs'
          nextErr = errFn nextXs
          shrunk = cfgShrink * stepSize
       in if nextErr < err
            then
              Right (Solution nextErr nextXs)
                : findSolution (stepSize * if firstTry then cfgGrow else 1) nextXs nextErr
            else Left shrunk : findStep False shrunk xs err xs'

gradient ::
  forall c t a.
  (Parameterized t, Num a, c (AD.Value a)) =>
  ErrorFunc c t ->
  t a ->
  t a
gradient errFn xs =
  zipWithT
    (\s x -> let AD.Value _ x' = errFn $ s xs' (AD.variable x) in x')
    (parameters xs')
    xs
  where
    xs' = AD.constant <$> xs

zipWithT :: (Traversable t, Foldable f) => (a -> b -> c) -> f a -> t b -> t c
zipWithT f xs =
  snd
    . mapAccumL
      ( \xs' y -> case xs' of
          [] -> error "Size mismatch!"
          (x : xs'') -> (xs'', f x y)
      )
      (toList xs)
