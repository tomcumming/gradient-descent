module Math.GradientDescent
  ( ErrorFunc,
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
  (c a, c (AD.Value a), Traversable t, Ord a, Num a) =>
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

zipWithT :: Traversable t => (a -> b -> c) -> t a -> t b -> t c
zipWithT f xs =
  snd
    . mapAccumL
      ( \xs' y -> case xs' of
          [] -> error "Size mismatch!"
          (x : xs'') -> (xs'', f x y)
      )
      (toList xs)

gradient ::
  forall c t a.
  (Traversable t, Num a, c (AD.Value a)) =>
  ErrorFunc c t ->
  t a ->
  t a
gradient errFn xs = snd $ mapAccumL go 0 xs
  where
    go :: Int -> a -> (Int, a)
    go idx1 _ =
      let params =
            snd $
              mapAccumL
                (\idx2 x -> (succ idx2, (if idx1 == idx2 then AD.variable else AD.constant) x))
                0
                xs
          (AD.Value _ x') = errFn params
       in (succ idx1, x')
