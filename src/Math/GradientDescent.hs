module Math.GradientDescent
  ( ErrorFunc,
    Config (..),
    Solution (..),
    defaultConfig,
    gradientDescent,
    gradient,
  )
where

import Data.Functor.WithIndex (imap)
import Data.Sized (Sized)
import Data.Sized qualified as Sized
import Data.Vector (Vector)
import GHC.TypeLits (KnownNat)
import Math.AD qualified as AD

type ErrorFunc c n = forall b. c b => Sized Vector n b -> b

data Config n a = Config
  { cfgInitialStepSize :: a,
    cfgGrow :: a,
    cfgShrink :: a,
    -- | Normalize parameters after moving down gradient
    cfgNormalize :: Sized Vector n a -> Sized Vector n a
  }

data Solution n a = Solution
  { solError :: a,
    solParams :: Sized Vector n a
  }
  deriving (Show)

defaultConfig :: Fractional a => Config n a
defaultConfig =
  Config
    { cfgInitialStepSize = 1,
      cfgGrow = 2,
      cfgShrink = 0.5,
      cfgNormalize = id
    }

-- | Returns a stream of either a new step size (after failing) or a better solution
gradientDescent ::
  forall c n a.
  (c a, c (AD.Value a), KnownNat n, Ord a, Num a) =>
  Config n a ->
  ErrorFunc c n ->
  Sized Vector n a ->
  [Either a (Solution n a)]
gradientDescent Config {..} errFn initial =
  let initialErr = errFn initial
   in Right (Solution initialErr initial) : findSolution cfgInitialStepSize initial (errFn initial)
  where
    findSolution :: a -> Sized Vector n a -> a -> [Either a (Solution n a)]
    findSolution stepSize xs err = findStep True stepSize xs err (gradient @c errFn xs)

    findStep :: Bool -> a -> Sized Vector n a -> a -> Sized Vector n a -> [Either a (Solution n a)]
    findStep firstTry stepSize xs err xs' =
      let nextXs = cfgNormalize $ Sized.zipWithSame (\x x' -> x - x' * stepSize) xs xs'
          nextErr = errFn nextXs
          shrunk = cfgShrink * stepSize
       in if nextErr < err
            then
              Right (Solution nextErr nextXs)
                : findSolution (stepSize * if firstTry then cfgGrow else 1) nextXs nextErr
            else Left shrunk : findStep False shrunk xs err xs'

gradient ::
  forall c n a.
  (c a, c (AD.Value a), KnownNat n, Num a) =>
  ErrorFunc c n ->
  Sized Vector n a ->
  Sized Vector n a
gradient errFn xs = (\(AD.Value _ x') -> x') <$> vals
  where
    vals =
      imap
        ( \idx _ ->
            let ps = imap (\idx2 x -> (if idx == idx2 then AD.variable else AD.constant) x) xs
             in errFn ps
        )
        xs
