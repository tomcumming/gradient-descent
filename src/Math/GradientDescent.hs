module Math.GradientDescent
  ( ErrorFunc,
    Config (..),
    gradientDescent,
  )
where

import Data.Functor.WithIndex (imap)
import Data.Sized (Sized)
import Data.Sized qualified as Sized
import Data.Vector (Vector)
import GHC.TypeLits (KnownNat)
import Math.AD qualified as AD

type ErrorFunc n = forall b. Fractional b => Sized Vector n b -> b

data Config a = Config
  { cfgInitialStepSize :: a,
    cfgGrow :: a,
    cfgShrink :: a
  }
  deriving (Show)

data Solution n a = Solution
  { solError :: a,
    solParams :: Sized Vector n a
  }
  deriving (Show)

-- | Returns a stream of either a new step size (after failing) or a better solution
gradientDescent ::
  forall n a.
  (KnownNat n, Fractional a, Ord a) =>
  Config a ->
  ErrorFunc n ->
  Sized Vector n a ->
  [Either a (Solution n a)]
gradientDescent Config {..} errFn initial = go cfgInitialStepSize initial (errFn initial)
  where
    go :: a -> Sized Vector n a -> a -> [Either a (Solution n a)]
    go stepSize xs err = go2 True stepSize xs err (gradient errFn xs)

    go2 :: Bool -> a -> Sized Vector n a -> a -> Sized Vector n a -> [Either a (Solution n a)]
    go2 firstTry stepSize xs err xs' =
      let nextXs = Sized.zipWith (\x x' -> x + x' * stepSize) xs xs'
          nextErr = errFn nextXs
          shrunk = cfgShrink * stepSize
       in if nextErr < err
            then
              Right (Solution nextErr nextXs)
                : go (stepSize * if firstTry then cfgGrow else 1) nextXs nextErr
            else Left shrunk : go2 False shrunk xs err xs'

gradient :: (KnownNat n, Fractional a) => ErrorFunc n -> Sized Vector n a -> Sized Vector n a
gradient errFn xs = (\(AD.Value _ x') -> x') <$> vals
  where
    vals =
      imap
        ( \idx _ ->
            let ps = imap (\idx2 x -> (if idx == idx2 then AD.variable else AD.constant) x) xs
             in errFn ps
        )
        xs
