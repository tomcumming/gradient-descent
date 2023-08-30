{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{- The following is a toy example to find some target transformation that turns
   one triangle into another. It is an example of a function with multiple
   parameters of differing types.
-}
module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (forM_, join)
import Data.Data ((:~:) (..))
import Data.Functor.Const qualified as C
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Math.GradientDescent (Add (..), Config (..), Scale (..), State (..), adaptiveStepSize, step)
import Math.GradientDescent.Params (ParamFn (..), ParamId (..), Params (..), indexParams, mapParams)

data Zero = Zero deriving (Show)
data V2 = V2 Double Double deriving (Show)

instance Add Zero where add _ _ = Zero
instance Scale Double Zero where scale _ _ = Zero
instance Add Double where add = (+)
instance Scale Double Double where scale = (*)
instance Add V2 where add (V2 a b) (V2 c d) = V2 (a + c) (b + d)
instance Scale Double V2 where scale k (V2 a b) = V2 (k * a) (k * b)

data Transform f = Transform
  { txScale :: f Double,
    txTranslate :: f V2
  }

deriving instance (Show (f Double), Show (f V2)) => Show (Transform f)

data Typed (f :: Type -> Type) where
  TypedZero :: f Zero -> Typed f
  TypedScalar :: f Double -> Typed f
  TypedV2 :: f V2 -> Typed f

class IntoTyped t where intoTyped :: f t -> Typed f
instance IntoTyped Zero where intoTyped = TypedZero
instance IntoTyped Double where intoTyped = TypedScalar
instance IntoTyped V2 where intoTyped = TypedV2

deriving instance (Show (f Zero), Show (f Double), Show (f V2)) => Show (Typed f)

class (IntoTyped a, Scale Double a) => Cs a
instance (IntoTyped a, Scale Double a) => Cs a

instance Params Cs Transform where
  traverseParams f Transform {..} = Transform <$> f txScale <*> f txTranslate
  zipParams f a b =
    Transform
      (f (txScale a) (txScale b))
      (f (txTranslate a) (txTranslate b))
  idParams = Transform (ParamId txScale) (ParamId txTranslate)

instance Add (Transform Identity) where add = zipParams (liftA2 add)
instance Scale Double (Transform Identity) where scale k = mapParams (fmap (scale k))

data Expr t where
  Const :: t -> Expr t
  Var :: ParamId Transform t -> Expr t
  Add :: Expr t -> Expr t -> Expr t
  Scale :: Expr Double -> Expr t -> Expr t
  Dot :: Expr V2 -> Expr V2 -> Expr Double

type Index = Word

indexed :: Transform (C.Const Index)
indexed = fst $ indexParams 0

-- | Calculate the derivative of an expression for some variable
diff :: IntoTyped t => Index -> Expr t -> Either String (Typed Expr)
diff dv = \case
  Const _ -> pure . intoTyped $ Const Zero
  Var (ParamId v)
    | v indexed == C.Const dv -> pure . intoTyped $ Const @Double 1
    | otherwise -> pure . intoTyped $ Const Zero
  Add l r -> join $ doAdd <$> diff dv l <*> diff dv r
  Scale k x ->
    join $
      doAdd
        <$> join (doInner <$> diff dv k <*> pure (intoTyped x))
        <*> join (doInner (intoTyped k) <$> diff dv x)
  Dot l r ->
    join $
      doAdd
        <$> join (doInner <$> diff dv l <*> pure (intoTyped r))
        <*> join (doInner (intoTyped l) <$> diff dv r)
  where
    doAdd :: Typed Expr -> Typed Expr -> Either String (Typed Expr)
    doAdd l r = case (l, r) of
      (TypedZero _, _) -> pure r
      (_, TypedZero _) -> pure l
      (TypedScalar ld, TypedScalar rd) -> pure . intoTyped $ Add ld rd
      (TypedV2 lv, TypedV2 rv) -> pure . intoTyped $ Add lv rv
      _ -> Left $ "Adding mixed types"

    doInner :: Typed Expr -> Typed Expr -> Either String (Typed Expr)
    doInner l r = case (l, r) of
      (TypedZero _, _) -> pure . intoTyped $ Const Zero
      (_, TypedZero _) -> pure . intoTyped $ Const Zero
      (TypedScalar ld, TypedScalar rd) -> pure . intoTyped $ Scale ld rd
      (TypedScalar ld, TypedV2 rv) -> pure . intoTyped $ Scale ld rv
      (TypedV2 lv, TypedScalar rd) -> pure . intoTyped $ Scale rd lv
      (TypedV2 lv, TypedV2 rv) -> pure . intoTyped $ Dot lv rv

eval :: Scale Double t => Transform Identity -> Expr t -> t
eval ps = \case
  Const c -> c
  Var (ParamId v) -> runIdentity $ v ps
  Add l r -> add (eval ps l) (eval ps r)
  Scale k x -> scale (eval ps k) (eval ps x)
  Dot l r -> case (eval ps l, eval ps r) of
    (V2 a b, V2 c d) -> a * c + b * d

-- | Scale then translate a polygon
transformPoly :: Transform Expr -> NE.NonEmpty (Expr V2) -> NE.NonEmpty (Expr V2)
transformPoly Transform {..} = fmap (Add txTranslate . Scale txScale)

-- | Sum of all points distance squared
comparePoly :: NE.NonEmpty (Expr V2) -> NE.NonEmpty (Expr V2) -> Expr Double
comparePoly xs ys =
  foldl1 Add
    . fmap ((\x -> Dot x x) . (\(x, y) -> Add x (Scale (Const (-1)) y)))
    $ NE.zip xs ys

initialTransform :: Transform Identity
initialTransform = Transform (Identity 1) (Identity $ V2 0 0)

errorFn :: Expr Double
errorFn =
  comparePoly
    (transformPoly targetTransform triangle)
    (transformPoly currentTransform triangle)
  where
    targetTransform = Transform (Const 2) (Const $ V2 2 3)
    currentTransform :: Transform Expr = mapParams Var idParams
    triangle = fmap Const $ V2 0 1 NE.:| [V2 (-1) 0, V2 1 0]

tryCast :: forall f a. IntoTyped a => Typed f -> Either String (f a)
tryCast fx = case (intoTyped (Refl @a), fx) of
  (TypedZero Refl, TypedZero x) -> pure x
  (TypedScalar Refl, TypedScalar x) -> pure x
  (TypedV2 Refl, TypedV2 x) -> pure x
  _ -> Left "Type mismatch"

gradient :: Either String (Transform (ParamFn Transform Identity))
gradient = traverseParams go indexed
  where
    go (C.Const idx) = go2 <$> (tryCast =<< diff idx errorFn)
    go2 expr = ParamFn $ \ps -> Identity (eval ps expr)

main :: IO ()
main = do
  gradFn <-
    either
      (fail . (<> "Gradient fn: "))
      (\x -> pure $ \ps -> mapParams (($ ps) . unParamFn) x)
      gradient
  let cfg =
        Config
          { cfgScale = adaptiveStepSize @Double 1.5 0.5,
            cfgError = (`eval` errorFn),
            cfgGradient = gradFn,
            cfgNorm = id
          }
  let initialState =
        State
          { stScale = -1,
            stValues = initialTransform,
            stError = eval initialTransform errorFn,
            stGradient = gradFn initialTransform
          }
  let states = iterate (step cfg) initialState
  forM_ (takeWhile ((> 1e-5) . stError) states) $ \State {..} -> do
    print stValues
    putStrLn $ "StepSize: " <> show stScale
    putStrLn $ "Error: " <> show stError
    putStrLn ""
