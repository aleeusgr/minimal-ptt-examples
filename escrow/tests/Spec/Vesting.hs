{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Spec.Vesting( tests, VestingModel ) where

import Control.Lens hiding (both)
import Control.Monad (void, when)
import Data.Default (Default (def))
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Cardano.Api.Shelley (toPlutusData)
import Cardano.Node.Emulator.Params qualified as Params
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Ledger (Slot (..), minAdaTxOutEstimated)
import Ledger.Time (POSIXTime)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value

import Contract.Escrow hiding (Action (..))
import Contract.Escrow qualified as Impl
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx (fromData)
import PlutusTx.Monoid (inv)

import Cardano.Api hiding (Value)
import Test.QuickCheck as QC hiding ((.&&.))
import Test.QuickCheck.ContractModel (utxo)
import Test.QuickCheck.ContractModel.ThreatModel
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck hiding ((.&&.))

import Spec.Endpoints

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
				 } deriving (Eq, Show, Generic)

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
