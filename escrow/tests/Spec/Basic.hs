module Spec.Basic (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Plutus.Contract.Test

import Data.List
import Data.Ord

import Ledger.Address qualified as Address
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value qualified as Value
import Ledger.Typed.Scripts qualified as Scripts
import Cardano.Node.Emulator qualified as TimeSlot
import Data.Default (Default (def))

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]
-- Tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  , testCase "wallets" $ 1 @?= 2
  ]
