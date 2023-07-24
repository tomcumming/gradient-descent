module Main (main) where

import Asteroid qualified as Ast
import Math.AD qualified as AD
import Math.GradientDescent
import Math.GradientDescent.Gradient (ScoreFn)
import SinWave qualified as SW
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate (lt, (.$))
import Test.Falsify.Property (assert, gen)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Falsify (Property, testProperty)

firstSolution ::
  Show a =>
  [Either (Error a) (Step p Double a)] ->
  Either String (Solution p Double a)
firstSolution = \case
  [] -> Left "No solution found"
  Left e : _ -> Left $ show e
  Right (Miss _) : rest -> firstSolution rest
  Right (Improvement s) : _ -> Right s

testFunction ::
  forall p a.
  (Show (p a), Show a, Traversable p, IntoScalar Double a, AD.Zero a, AD.One a, Correct Double a) =>
  Double ->
  Int ->
  Gen.Gen (p a) ->
  ScoreFn p a ->
  Property ()
testFunction targetError maxSteps genParams scoreFn = do
  ps <- gen genParams
  let steps = take maxSteps $ gradientDescent defaultConfig scoreFn ps
  Solution {..} <-
    either
      (fail . show)
      pure
      $ firstSolution
      $ reverse steps
  assert $ lt .$ ("Solution Error", solError) .$ ("Target", targetError)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Solving"
      [ testProperty "Solve asteroid example" $
          testFunction 1e-10 50 Ast.genParams Ast.scoreFn,
        testProperty "Solve sinwave sum fitting" $
          testFunction 1e-10 20 SW.genParams SW.scoreFn
      ]
