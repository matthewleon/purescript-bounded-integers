module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Enum (enumFromTo)
import Data.Integer.Bounded.Nat (NatBelow, below)
import Data.Maybe (fromJust)
import Data.Typelevel.Num (D1, D2, D3, D11, toInt)
import Data.Vec (Vec, (+>), (!!), singleton)
import Partial.Unsafe (unsafePartial)

import Test.Unit (suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest $ suite "NatBelow tests" do
  let (testVec :: Vec D3 String) = "foo" +> "bar" +> singleton "bam"
      (i0 :: NatBelow D1) = unsafePartial $ fromJust $ below 0
      (i1 :: NatBelow D2) = unsafePartial $ fromJust $ below 1
      (i2 :: NatBelow D3) = unsafePartial $ fromJust $ below 2
  test "i0" $ equal "foo" $ testVec !! i0
  test "i1" $ equal "bar" $ testVec !! i1
  test "i2" $ equal "bam" $ testVec !! i2

  test "enum" $ equal [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
              $ toInt <$> (enumFromTo bottom top :: Array (NatBelow D11))
