module Spec.Vesting( tests ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.ContractModel (utxo)
import Test.QuickCheck.ContractModel.ThreatModel
import Test.Tasty.HUnit qualified as HUnit

import Data.List
import Data.Ord

-- define a type to represent the expected expected state of the state of the ecosystem.
-- compare Escrow L63
data VestingModel = VestingModel { _contributions :: Map Wallet Value.Value
                                 , _targets       :: Map Wallet Value.Value
				 } deriving (Eq, Show, CM.Generic)

makeLenses ''VestingModel

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
  [ HUnit.testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] HUnit.@?= GT
  ]
