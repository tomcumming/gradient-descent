module Math.GradientDescent.Unsized
  ( ErrorFunc,
    Config (..),
    Solution (..),
    defaultConfig,
    gradientDescent,
  )
where

import Data.Bifunctor (second)
import Data.Sized qualified as Sized
import Data.Type.Natural (KnownNat, SNat, withKnownNat)
import Data.Vector qualified as V
import Math.AD qualified as AD
import Math.GradientDescent qualified as GD

type ErrorFunc c = forall b. c b => V.Vector b -> b

data Config a = Config
  { cfgInitialStepSize :: a,
    cfgGrow :: a,
    cfgShrink :: a,
    -- | Normalize parameters after moving down gradient
    cfgNormalize :: V.Vector a -> V.Vector a
  }

data Solution a = Solution
  { solError :: a,
    solParams :: V.Vector a
  }
  deriving (Show)

defaultConfig :: Fractional a => Config a
defaultConfig =
  Config
    { cfgInitialStepSize = 1,
      cfgGrow = 2,
      cfgShrink = 0.5,
      cfgNormalize = id
    }

-- | Returns a stream of either a new step size (after failing) or a better solution
gradientDescent ::
  forall c a.
  (c a, c (AD.Value a), Ord a, Num a) =>
  Config a ->
  ErrorFunc c ->
  V.Vector a ->
  [Either a (Solution a)]
gradientDescent Config {..} errFn initial = case Sized.toSomeSized initial of
  Sized.SomeSized sn sv -> withKnownNat sn (go sn sv)
  where
    cfg :: SNat n -> GD.Config n a
    cfg sn =
      GD.Config
        { cfgInitialStepSize,
          cfgGrow,
          cfgShrink,
          cfgNormalize = Sized.unsafeToSized sn . cfgNormalize . Sized.unsized
        }

    sErrFn :: SNat n -> GD.ErrorFunc c n
    sErrFn _ = errFn . Sized.unsized

    go ::
      forall n.
      KnownNat n =>
      SNat n ->
      Sized.Sized V.Vector n a ->
      [Either a (Solution a)]
    go sn sv =
      second
        ( \(GD.Solution {..}) ->
            Solution {solError, solParams = Sized.unsized solParams}
        )
        <$> GD.gradientDescent @c @n @a (cfg sn) (sErrFn sn) sv
