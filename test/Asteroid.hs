module Asteroid
  ( genParams,
    scoreFn,
  )
where

import Asteroid.Math
import Data.Maybe (fromMaybe)
import Math.AD qualified as AD
import Math.GradientDescent
import Math.GradientDescent.Gradient (HasParams (param), ScoreFn)
import Test.Falsify.Generator qualified as Gen

asteroidDetectedAt :: MV
asteroidDetectedAt = V2 10 50

asteroidVelocity :: MV
asteroidVelocity = V2 0 (-1)

secondsToImpact :: Double
secondsToImpact =
  abs
    . fromMaybe (error "Impact time must be scalar")
    . intoScalar
    $ inner' asteroidVelocity asteroidDetectedAt

maxMissileSpeed :: Double
maxMissileSpeed = 10

data Params a = Params
  { prmMissileVel :: a,
    prmDetonationTime :: a
  }
  deriving (Show, Functor, Foldable, Traversable)

-- We need to find the correct missile params to intercept the asteroid!
-- Unfortunately the Aliens have used a brain-scrambling ray and we are
-- unable to solve analytically
scoreFn :: ScoreFn Params MV
scoreFn Params {..} = do
  missileVel <- param prmMissileVel
  detonationTime <- param prmDetonationTime

  -- At detonation time:
  let missilePos = inner missileVel detonationTime
  let asteroidPos =
        add
          (AD.constant asteroidDetectedAt)
          (inner (AD.constant asteroidVelocity) detonationTime)

  let dist = sub missilePos asteroidPos
  let distanceError = inner dist dist

  pure
    $ add
      distanceError
    $ add (missileSpeedError missileVel) (timeLimitError detonationTime)

timeLimitError :: AD.Value MV -> AD.Value MV
timeLimitError = \case
  dt@(AD.Value (Scalar detTime) _)
    | detTime > secondsToImpact,
      ex <- sub dt (AD.constant $ Scalar $ detTime - secondsToImpact) ->
        inner ex ex
  _ -> AD.constant (Scalar 0)

missileSpeedError :: AD.Value MV -> AD.Value MV
missileSpeedError = \case
  ms@(AD.Value msz _)
    | Scalar s2 <- inner' msz msz,
      speed <- sqrt s2,
      speed > maxMissileSpeed,
      ex <- sub ms (inner ms (AD.constant $ Scalar (maxMissileSpeed / speed))) ->
        inner ex ex
  _ -> AD.constant (Scalar 0)

genParams :: Gen.Gen (Params MV)
genParams = do
  -- TODO
  pure $
    Params
      { prmMissileVel = V2 0 0,
        prmDetonationTime = Scalar 0
      }
