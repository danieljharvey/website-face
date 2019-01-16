module Test.Main where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Effect.Aff (launchAff_)

import Tests.EffectAndAff as EffectAndAff

main :: Effect Unit
main = runTest do
  EffectAndAff.tests
