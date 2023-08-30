module Math.GradientDescent
  ( Add (..),
    Scale (..),
    Config (..),
    State (..),
    step,
    adaptiveStepSize,
    constantStepSize,
  )
where

class Add a where
  add :: a -> a -> a

class Add a => Scale s a where
  scale :: s -> a -> a

data Config p s e = Config
  { -- | Rescale after hit or miss
    cfgScale :: Ordering -> s -> s,
    -- | Error scoring function
    cfgError :: p -> e,
    -- | Calculate gradient
    cfgGradient :: p -> p,
    -- | Normalise parameters, @id@ is a fine default
    cfgNorm :: p -> p
  }

data State p s e = State
  { -- | A negative number,
    --   we scale the gradient by this, then add to current solution
    stScale :: s,
    stValues :: p,
    stGradient :: p,
    stError :: e
  }

step ::
  forall p s e.
  (Scale s p, Ord e) =>
  Config p s e ->
  State p s e ->
  State p s e
step Config {..} State {..} = case errorCmp of
  LT ->
    State
      { stValues = nextParamVals,
        stGradient = cfgGradient nextParamVals,
        stError = nextError,
        stScale = nextScale
      }
  _ -> State {stValues, stGradient, stError, stScale = nextScale}
  where
    nextParamVals = cfgNorm . add stValues $ scale stScale stGradient
    nextError = cfgError nextParamVals
    errorCmp = compare nextError stError
    nextScale = cfgScale errorCmp stScale

-- | Grow and shrink values for adaptive step size
adaptiveStepSize :: Num s => s -> s -> Ordering -> s -> s
adaptiveStepSize grow shrink = \case
  LT -> (* grow)
  _ -> (* shrink)

constantStepSize :: Ordering -> s -> s
constantStepSize _ = id
