module Math.GradientDescent.Params
  ( ParamId (..),
    ParamFn (..),
    Params (..),
    mapParams,
    indexParams,
  )
where

import Control.Monad.Identity (Identity (..))
import Control.Monad.State (runState, state)
import Data.Functor.Const (Const (..))

newtype ParamId p a = ParamId {unParamId :: forall f. p f -> f a}
newtype ParamFn p f a = ParamFn {unParamFn :: p f -> f a}

class Params c p | p -> c where
  traverseParams ::
    Applicative m => (forall a. c a => f a -> m (g a)) -> p f -> m (p g)
  zipParams :: (forall a. c a => f a -> g a -> h a) -> p f -> p g -> p h
  idParams :: p (ParamId p)

mapParams :: Params c p => (forall a. c a => f a -> g a) -> p f -> p g
mapParams f = runIdentity . traverseParams (Identity . f)

indexParams :: (Enum idx, Params c p) => idx -> (p (Const idx), idx)
indexParams =
  runState
    (traverseParams (const $ state (\i -> (Const i, succ i))) idParams)
